namespace Lfsi.App

open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading
open Consolonia
open Consolonia.Themes
open Lfsi.Core

module NativeEnvironment =
    [<Literal>]
    let EscDelayName = "ESCDELAY"

    [<Literal>]
    let DefaultEscDelay = "1"

    [<DllImport("libc")>]
    extern int setenv(string name, string value, int overwrite)

    // .NET environment mutation is not visible to native getenv on Unix for some reason
    let setIfMissing name value =
        if
            not (OperatingSystem.IsWindows())
            && (Environment.GetEnvironmentVariable(name) |> String.IsNullOrWhiteSpace)
        then
            setenv (name, value, 1) |> ignore

type QuitConfirmation =
    | Hidden
    | Arming
    | Armed

type HeaderAction = { Key: string; Label: string }

type HeaderModel =
    { AppName: string
      CellPosition: string option
      Mode: string
      IsDirty: bool
      Actions: HeaderAction list }

type WordJumpDirection =
    | PreviousWord
    | NextWord

module NotebookHeader =
    let private renderAction action = action.Key + " " + action.Label

    let render model =
        let position =
            model.CellPosition
            |> Option.map (fun value -> "  " + value)
            |> Option.defaultValue ""

        let dirty = if model.IsDirty then " *" else ""

        let actions = model.Actions |> List.map renderAction |> String.concat "  "

        model.AppName + position + dirty + "  " + model.Mode + "  " + actions

type NotebookWindow(path: string, configuration: LfsiConfiguration) as this =
    inherit Window(Title = "lfsi notebook", WindowState = WindowState.Maximized)

    let initialParsed, initialWriteTimeUtc = FilePersistence.load path
    let persistenceMode = AutoReloadWhenClean

    let fsiWorkingDirectory =
        let directory = Path.GetDirectoryName path

        if String.IsNullOrWhiteSpace directory then
            Environment.CurrentDirectory
        else
            directory

    let fsi = new FsiSession(fsiWorkingDirectory, configuration.Fsi.ExecutablePath)
    let quitConfirmationMessage = "Press Ctrl+C again to quit, or Esc to cancel."
    let mutable parsed = initialParsed
    let mutable lastWriteTimeUtc = initialWriteTimeUtc
    let mutable quitConfirmation = Hidden
    let mutable selectedIndex = 0
    let mutable cells = parsed.Document.Cells
    let mutable isDirty = false
    let mutable isEditing = false
    let mutable isRunning = false
    let mutable hasExternalChanges = false
    let mutable selectedEditor: TextBox option = None
    let mutable cellClipboard: NotebookCell option = None

    let formattingStatus () =
        if parsed.FormattingDiagnostics.IsEmpty then
            "FSharp.Formatting parse: ok"
        else
            "FSharp.Formatting parse: " + String.concat "; " parsed.FormattingDiagnostics

    let restoreStatus (status: TextBlock) = status.Text <- formattingStatus ()

    let cellKindLabel kind =
        match kind with
        | CellKind.Markdown -> "markdown"
        | CellKind.Code -> "fsx"

    let addCellOutputs theme errorBrush visualOutputCache imageBackend (body: StackPanel) cell =
        if not cell.Outputs.IsEmpty then
            body.Children.Add(
                OutputRendering.renderOutputs theme errorBrush visualOutputCache imageBackend cell.Outputs
            )
            |> ignore

    let renderCellSourceControl theme renderCodeSource cell =
        match cell.Kind with
        | CellKind.Code -> renderCodeSource (cell.Source.TrimEnd())
        | CellKind.Markdown ->
            TextBlock(Text = cell.Source.TrimEnd(), Foreground = theme.Text, TextWrapping = TextWrapping.NoWrap)
            :> Control

    let addCellPreview
        theme
        selectedBrush
        errorBrush
        visualOutputCache
        imageBackend
        renderCellSource
        selectedIndex
        (cellStack: StackPanel)
        index
        cell
        =
        let isSelected = index = selectedIndex
        let body = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)

        body.Children.Add(
            TextBlock(
                Text = sprintf "[%02d] %s" (index + 1) (cellKindLabel cell.Kind),
                Foreground = (if isSelected then theme.Accent else theme.Muted)
            )
        )
        |> ignore

        body.Children.Add(renderCellSource cell) |> ignore

        addCellOutputs theme errorBrush visualOutputCache imageBackend body cell

        let frame =
            Border(
                Background = (if isSelected then selectedBrush else theme.Panel),
                Padding = Thickness(1.0),
                Margin = Thickness(0.0, 0.0, 0.0, 1.0),
                Child = body
            )

        cellStack.Children.Add(frame) |> ignore
        frame

    let tryGetWordJumpDirection (args: KeyEventArgs) =
        let hasModifier modifier = args.KeyModifiers.HasFlag modifier

        if hasModifier KeyModifiers.Shift then
            None
        elif hasModifier KeyModifiers.Alt && args.Key = Key.B then
            Some PreviousWord
        elif hasModifier KeyModifiers.Alt && args.Key = Key.F then
            Some NextWord
        elif
            (hasModifier KeyModifiers.Control || hasModifier KeyModifiers.Alt)
            && args.Key = Key.Left
        then
            Some PreviousWord
        elif
            (hasModifier KeyModifiers.Control || hasModifier KeyModifiers.Alt)
            && args.Key = Key.Right
        then
            Some NextWord
        else
            None

    let previousWordBoundary caretIndex (text: string) =
        if caretIndex <= 0 then
            0
        else
            let mutable index = Math.Min(caretIndex, text.Length) - 1

            while index >= 0 && Char.IsWhiteSpace text.[index] do
                index <- index - 1

            while index >= 0 && not (Char.IsWhiteSpace text.[index]) do
                index <- index - 1

            index + 1

    let nextWordBoundary caretIndex (text: string) =
        let mutable index =
            if caretIndex < 0 then 0
            elif caretIndex > text.Length then text.Length
            else caretIndex

        while index < text.Length && not (Char.IsWhiteSpace text.[index]) do
            index <- index + 1

        while index < text.Length && Char.IsWhiteSpace text.[index] do
            index <- index + 1

        index

    let setEditorCaret index (editor: TextBox) =
        editor.CaretIndex <- index
        editor.SelectionStart <- index
        editor.SelectionEnd <- index

    let moveEditorCaretByWord direction (editor: TextBox) =
        let text = editor.Text |> Option.ofObj |> Option.defaultValue ""

        let nextIndex =
            match direction with
            | PreviousWord -> previousWordBoundary editor.CaretIndex text
            | NextWord -> nextWordBoundary editor.CaretIndex text

        setEditorCaret nextIndex editor
        // TextBox may adjust the caret after KeyDown; post once to win that ordering.
        Dispatcher.UIThread.Post(fun () -> setEditorCaret nextIndex editor)

    let addEditableCell
        theme
        selectedBrush
        errorBrush
        visualOutputCache
        imageBackend
        renderCodeSource
        updateHighlightedCode
        (cellStack: StackPanel)
        onTextChanged
        index
        cell
        =
        let body = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)

        body.Children.Add(
            TextBlock(Text = sprintf "[%02d] %s" (index + 1) (cellKindLabel cell.Kind), Foreground = theme.Accent)
        )
        |> ignore

        let editor =
            TextBox(
                Text = cell.Source,
                AcceptsReturn = true,
                TextWrapping = TextWrapping.NoWrap,
                Foreground = theme.Text,
                Background = theme.Dark,
                MinHeight = 3.0
            )

        let highlightedEditor =
            match cell.Kind with
            | CellKind.Code -> Some(renderCodeSource (editor.Text |> Option.ofObj |> Option.defaultValue ""))
            | CellKind.Markdown -> None

        editor.TextChanged.Add(fun _ ->
            let editorText = editor.Text |> Option.ofObj |> Option.defaultValue ""

            onTextChanged cell.Source editorText

            highlightedEditor
            |> Option.iter (fun block -> updateHighlightedCode editorText block))

        editor.AddHandler(
            InputElement.KeyDownEvent,
            (fun _ args ->
                match tryGetWordJumpDirection args with
                | Some direction ->
                    args.Handled <- true
                    moveEditorCaretByWord direction editor
                | None -> ()),
            RoutingStrategies.Tunnel,
            true
        )

        body.Children.Add(editor) |> ignore

        highlightedEditor
        |> Option.iter (fun block -> body.Children.Add block |> ignore)

        addCellOutputs theme errorBrush visualOutputCache imageBackend body cell

        cellStack.Children.Add(
            Border(
                Background = selectedBrush,
                Padding = Thickness(1.0),
                Margin = Thickness(0.0, 0.0, 0.0, 1.0),
                Child = body
            )
        )
        |> ignore

        editor

    let replaceCellSource selectedIndex source cells =
        cells
        |> List.mapi (fun index cell ->
            if index = selectedIndex then
                { cell with Source = source }
            else
                cell)

    let replaceCellOutput selectedIndex output cells =
        cells
        |> List.mapi (fun index cell ->
            if index = selectedIndex then
                { cell with Outputs = [ output ] }
            else
                cell)

    let insertCellAt index cell cells =
        let before, after = cells |> List.splitAt index
        before @ (cell :: after)

    let removeCellAt index cells =
        cells
        |> List.mapi (fun i cell -> i, cell)
        |> List.filter (fst >> (<>) index)
        |> List.map snd

    let cloneCell cell = { cell with Id = Guid.NewGuid() }

    let replaceCellAt index replacement cells =
        cells |> List.mapi (fun i cell -> if i = index then replacement else cell)

    do
        let theme =
            { Dark = SolidColorBrush(Color.FromRgb(18uy, 18uy, 18uy))
              Panel = SolidColorBrush(Color.FromRgb(28uy, 30uy, 34uy))
              Text = SolidColorBrush(Color.FromRgb(232uy, 232uy, 232uy))
              Muted = SolidColorBrush(Color.FromRgb(170uy, 176uy, 184uy))
              Accent = SolidColorBrush(Color.FromRgb(140uy, 190uy, 255uy)) }

        let syntaxHighlighting = SyntaxHighlighting.defaultMode

        let syntaxPalette: SyntaxHighlighting.Palette =
            { Default = theme.Text
              Keyword = SolidColorBrush(Color.FromRgb(255uy, 160uy, 210uy))
              String = SolidColorBrush(Color.FromRgb(180uy, 230uy, 150uy))
              Comment = theme.Muted
              Number = SolidColorBrush(Color.FromRgb(255uy, 205uy, 140uy))
              Operator = SolidColorBrush(Color.FromRgb(160uy, 210uy, 255uy))
              Preprocessor = SolidColorBrush(Color.FromRgb(210uy, 180uy, 255uy)) }

        let highlightedCodeCache =
            Dictionary<string, SyntaxHighlighting.HighlightSpan list>()

        let highlightedCodeSpans source =
            match highlightedCodeCache.TryGetValue source with
            | true, spans -> spans
            | false, _ ->
                let spans = SyntaxHighlighting.highlight syntaxHighlighting source
                highlightedCodeCache.[source] <- spans
                spans

        let renderCodeSource source =
            SyntaxHighlighting.textBlock syntaxPalette (highlightedCodeSpans source)

        let renderCellSource cell =
            renderCellSourceControl theme (renderCodeSource >> fun block -> block :> Control) cell

        let updateHighlightedCode source block =
            SyntaxHighlighting.updateTextBlock syntaxPalette (highlightedCodeSpans source) block

        let visualOutputService = ChromeCdpVisualOutputService()
        let visualOutputCache = MemoryVisualOutputCache visualOutputService

        let imageBackend, terminalImageLayer =
            match TerminalGraphics.currentEnvironment () |> TerminalGraphics.decide with
            | UseTerminalGraphics Kitty ->
                let backend = KittyImageBackend()
                backend :> ITerminalImageBackend, backend :> ITerminalImageLayer
            | UseTerminalGraphics protocol ->
                let backend = FallbackTerminalImageBackend protocol
                backend :> ITerminalImageBackend, backend :> ITerminalImageLayer
            | UseTextFallback reason ->
                let backend = FallbackTerminalImageBackend(Kitty, reason)
                backend :> ITerminalImageBackend, backend :> ITerminalImageLayer


        let selectedBrush = SolidColorBrush(Color.FromRgb(38uy, 72uy, 118uy))
        let errorBrush = SolidColorBrush(Color.FromRgb(255uy, 150uy, 150uy))
        let root = DockPanel(Background = theme.Dark)
        let header = DockPanel(Background = theme.Dark)

        let headerText =
            TextBlock(Foreground = theme.Accent, Background = theme.Dark, TextWrapping = TextWrapping.Wrap)

        let status =
            TextBlock(
                Text = formattingStatus (),
                Foreground = theme.Muted,
                Background = theme.Dark,
                TextWrapping = TextWrapping.Wrap
            )

        let setStatus message = status.Text <- message

        let quit () =
            match Application.Current.ApplicationLifetime with
            | :? IControlledApplicationLifetime as lifetime -> lifetime.Shutdown()
            | _ -> this.Close()

        let requestQuit () =
            match quitConfirmation with
            | Hidden ->
                quitConfirmation <- Arming
                setStatus quitConfirmationMessage

                Task
                    .Delay(1)
                    .ContinueWith(fun _ ->
                        if quitConfirmation = Arming then
                            quitConfirmation <- Armed)
                |> ignore
            | Arming -> setStatus quitConfirmationMessage
            | Armed -> quit ()

        let cancelQuitConfirmation () =
            quitConfirmation <- Hidden
            restoreStatus status

        let applySelectedEdit () =
            selectedEditor
            |> Option.iter (fun editor ->
                let currentSource =
                    cells
                    |> List.tryItem selectedIndex
                    |> Option.map _.Source
                    |> Option.defaultValue ""

                if editor.Text <> currentSource then
                    cells <- cells |> replaceCellSource selectedIndex editor.Text
                    isDirty <- true)

        let cellStack = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)
        let mutable selectedFrame: Control option = None

        let clearTerminalImages () = terminalImageLayer.Clear()

        let modeLabel () =
            if isRunning then "running"
            elif isEditing then "editing"
            else "selection"

        let selectedRunnableCell () =
            cells
            |> List.tryItem selectedIndex
            |> Option.filter (fun cell -> cell.Kind = CellKind.Code)

        let isSelectedEditor index = isEditing && index = selectedIndex

        let updateHeader () =
            let actions =
                [ if not isEditing then
                      { Key = "↑↓"; Label = "move" }
                      { Key = "Enter"; Label = "edit" }
                      { Key = "A"; Label = "above" }
                      { Key = "B"; Label = "below" }

                      { Key = "X/C/V"
                        Label = "cut/copy/paste" }

                      { Key = "M/Y"; Label = "markdown/code" }

                  if isEditing then
                      { Key = "Esc"; Label = "select" }

                  if selectedRunnableCell () |> Option.isSome then
                      { Key = "F5"; Label = "run" }

                  if isDirty && FilePersistence.canSave persistenceMode then
                      { Key = "Ctrl+S"; Label = "save" }

                  { Key = "Ctrl+C"; Label = "quit" } ]

            let cellPosition =
                if cells.IsEmpty then
                    Some "no cells"
                else
                    Some(sprintf "%d/%d" (selectedIndex + 1) cells.Length)

            let mode = if cells.IsEmpty then "selection" else modeLabel ()

            headerText.Text <-
                NotebookHeader.render
                    { AppName = "lfsi"
                      CellPosition = cellPosition
                      Mode = mode
                      IsDirty = isDirty
                      Actions = actions }

        let markDirty originalSource editedSource =
            if editedSource <> originalSource && not isDirty then
                isDirty <- true
                setStatus "Unsaved in-memory edits."
                updateHeader ()

        let rebuildCells () =
            clearTerminalImages ()
            selectedEditor <- None
            selectedFrame <- None
            cellStack.Children.Clear()

            cells
            |> List.iteri (fun index cell ->
                if isSelectedEditor index then
                    selectedEditor <-
                        Some(
                            addEditableCell
                                theme
                                selectedBrush
                                errorBrush
                                visualOutputCache
                                imageBackend
                                renderCodeSource
                                updateHighlightedCode
                                cellStack
                                markDirty
                                index
                                cell
                        )
                else
                    let frame =
                        addCellPreview
                            theme
                            selectedBrush
                            errorBrush
                            visualOutputCache
                            imageBackend
                            renderCellSource
                            selectedIndex
                            cellStack
                            index
                            cell

                    if index = selectedIndex then
                        selectedFrame <- Some frame)

            updateHeader ()

            selectedEditor
            |> Option.iter (fun editor ->
                Dispatcher.UIThread.Post(fun () ->
                    editor.Focus() |> ignore
                    editor.CaretIndex <- editor.Text.Length))

            selectedFrame
            |> Option.iter (fun frame -> Dispatcher.UIThread.Post(fun () -> frame.BringIntoView()))

        let moveSelection delta =
            if not isEditing && not cells.IsEmpty then
                let last = cells.Length - 1
                let next = Math.Clamp(selectedIndex + delta, 0, last)

                if next <> selectedIndex then
                    selectedIndex <- next
                    rebuildCells ()

        let beginEditing () =
            if not cells.IsEmpty && not isEditing && not isRunning then
                isEditing <- true
                rebuildCells ()

        let addCellBelow () =
            if not isEditing && not isRunning then
                let insertIndex = if cells.IsEmpty then 0 else selectedIndex + 1

                cells <- cells |> insertCellAt insertIndex (NotebookCell.create CellKind.Code "")
                selectedIndex <- insertIndex
                isDirty <- true
                setStatus "Added code cell below."
                rebuildCells ()

        let addCellAbove () =
            if not isEditing && not isRunning then
                let insertIndex = if cells.IsEmpty then 0 else selectedIndex

                cells <- cells |> insertCellAt insertIndex (NotebookCell.create CellKind.Code "")
                selectedIndex <- insertIndex
                isDirty <- true
                setStatus "Added code cell above."
                rebuildCells ()

        let copyCell () =
            if not isEditing && not isRunning then
                cells
                |> List.tryItem selectedIndex
                |> Option.iter (fun cell ->
                    cellClipboard <- Some cell
                    setStatus "Copied cell.")

        let cutCell () =
            if not isEditing && not isRunning then
                cells
                |> List.tryItem selectedIndex
                |> Option.iter (fun cell ->
                    cellClipboard <- Some cell
                    cells <- cells |> removeCellAt selectedIndex

                    selectedIndex <-
                        if cells.IsEmpty then
                            0
                        else
                            Math.Clamp(selectedIndex, 0, cells.Length - 1)

                    isDirty <- true
                    setStatus "Cut cell."
                    rebuildCells ())

        let pasteCellBelow () =
            if not isEditing && not isRunning then
                cellClipboard
                |> Option.iter (fun cell ->
                    let insertIndex = if cells.IsEmpty then 0 else selectedIndex + 1

                    cells <- cells |> insertCellAt insertIndex (cloneCell cell)
                    selectedIndex <- insertIndex
                    isDirty <- true
                    setStatus "Pasted cell below."
                    rebuildCells ())

        let convertSelectedCell kind =
            if not isEditing && not isRunning then
                cells
                |> List.tryItem selectedIndex
                |> Option.filter (fun cell -> cell.Kind <> kind)
                |> Option.iter (fun cell ->
                    cells <- cells |> replaceCellAt selectedIndex { cell with Kind = kind; Outputs = [] }
                    isDirty <- true

                    setStatus (
                        match kind with
                        | CellKind.Markdown -> "Converted cell to Markdown."
                        | CellKind.Code -> "Converted cell to code."
                    )

                    rebuildCells ())

        let reloadFromDisk message =
            let nextParsed, nextWriteTimeUtc = FilePersistence.load path
            parsed <- nextParsed
            lastWriteTimeUtc <- nextWriteTimeUtc
            cells <- parsed.Document.Cells
            highlightedCodeCache.Clear()

            selectedIndex <-
                if cells.IsEmpty then
                    0
                else
                    Math.Clamp(selectedIndex, 0, cells.Length - 1)

            isDirty <- false
            isEditing <- false
            hasExternalChanges <- false
            setStatus message
            rebuildCells ()

        let saveToDisk () =
            if FilePersistence.canSave persistenceMode then
                applySelectedEdit ()

                let saveStatus =
                    if hasExternalChanges then
                        "Saved; external file changes overwritten."
                    else
                        "Saved."

                let nextParsed, nextWriteTimeUtc = FilePersistence.save path cells
                parsed <- nextParsed
                lastWriteTimeUtc <- nextWriteTimeUtc
                cells <- parsed.Document.Cells
                highlightedCodeCache.Clear()

                selectedIndex <-
                    if cells.IsEmpty then
                        0
                    else
                        Math.Clamp(selectedIndex, 0, cells.Length - 1)

                isDirty <- false
                hasExternalChanges <- false
                setStatus saveStatus
                rebuildCells ()
            else
                setStatus "Saving is disabled."

        let checkExternalChange () =
            let fileChanged = FilePersistence.hasChanged path lastWriteTimeUtc

            match
                FilePersistence.decideExternalChange persistenceMode fileChanged isDirty isEditing hasExternalChanges
            with
            | IgnoreExternalChange -> ()
            | ReloadExternalChange -> reloadFromDisk "Reloaded external file changes."
            | KeepInMemoryAndNotify ->
                hasExternalChanges <- true
                setStatus "File changed on disk; in-memory edits kept."

        let endEditing () =
            if isEditing then
                applySelectedEdit ()
                isEditing <- false

                if hasExternalChanges && not isDirty then
                    reloadFromDisk "Reloaded external file changes."
                else
                    rebuildCells ()

        let runSelectedAsync () =
            task {
                match selectedRunnableCell () with
                | Some cell when not isRunning ->
                    isRunning <- true
                    applySelectedEdit ()
                    isEditing <- false
                    setStatus "Running selected cell..."
                    rebuildCells ()

                    try
                        let! result = fsi.ExecuteAsync(cell.Source, CancellationToken.None)

                        do!
                            Dispatcher.UIThread.InvokeAsync(fun () ->
                                cells <- cells |> replaceCellOutput selectedIndex result.Output
                                isRunning <- false
                                setStatus "Ready"
                                rebuildCells ())
                    with ex ->
                        do!
                            Dispatcher.UIThread.InvokeAsync(fun () ->
                                cells <- cells |> replaceCellOutput selectedIndex (NotebookOutput.Error ex.Message)
                                isRunning <- false
                                setStatus "Execution failed."
                                rebuildCells ())
                | _ -> ()
            }

        let scroll =
            ScrollViewer(Content = cellStack, Background = theme.Dark, Focusable = false)

        scroll.ScrollChanged.Add(fun _ ->
            clearTerminalImages ()
            cellStack.InvalidateVisual())


        DockPanel.SetDock(header, Dock.Top)
        DockPanel.SetDock(status, Dock.Bottom)
        header.Children.Add(headerText) |> ignore
        root.Children.Add(header) |> ignore
        root.Children.Add(status) |> ignore
        root.Children.Add(scroll) |> ignore

        base.RequestedThemeVariant <- Styling.ThemeVariant.Dark
        base.Background <- theme.Dark
        base.Content <- root

        rebuildCells ()

        match persistenceMode with
        | NoPersistence -> ()
        | AutoReloadWhenClean ->
            let externalChangeTimer = DispatcherTimer()
            externalChangeTimer.Interval <- TimeSpan.FromSeconds 1.0
            externalChangeTimer.Tick.Add(fun _ -> checkExternalChange ())
            externalChangeTimer.Start()

        this.AddHandler(
            InputElement.KeyDownEvent,
            (fun _ args ->
                if args.KeyModifiers = KeyModifiers.Control && args.Key = Key.C then
                    args.Handled <- true
                    requestQuit ()
                elif args.KeyModifiers = KeyModifiers.Control && args.Key = Key.S then
                    args.Handled <- true
                    saveToDisk ()
                elif args.Key = Key.F5 && (selectedRunnableCell () |> Option.isSome) then
                    args.Handled <- true
                    runSelectedAsync () |> ignore
                elif args.Key = Key.A && args.KeyModifiers = KeyModifiers.None && not isEditing then
                    args.Handled <- true
                    addCellAbove ()
                elif args.Key = Key.B && args.KeyModifiers = KeyModifiers.None && not isEditing then
                    args.Handled <- true
                    addCellBelow ()
                elif args.Key = Key.X && args.KeyModifiers = KeyModifiers.None && not isEditing then
                    args.Handled <- true
                    cutCell ()
                elif args.Key = Key.C && args.KeyModifiers = KeyModifiers.None && not isEditing then
                    args.Handled <- true
                    copyCell ()
                elif args.Key = Key.V && args.KeyModifiers = KeyModifiers.None && not isEditing then
                    args.Handled <- true
                    pasteCellBelow ()
                elif args.Key = Key.M && args.KeyModifiers = KeyModifiers.None && not isEditing then
                    args.Handled <- true
                    convertSelectedCell CellKind.Markdown
                elif args.Key = Key.Y && args.KeyModifiers = KeyModifiers.None && not isEditing then
                    args.Handled <- true
                    convertSelectedCell CellKind.Code
                elif args.Key = Key.Down && not isEditing then
                    args.Handled <- true
                    moveSelection 1
                elif args.Key = Key.Up && not isEditing then
                    args.Handled <- true
                    moveSelection -1
                elif args.Key = Key.Enter && not isEditing then
                    args.Handled <- true
                    beginEditing ()
                elif args.Key = Key.Escape && isEditing then
                    args.Handled <- true
                    endEditing ()
                elif args.Key = Key.Escape && quitConfirmation <> Hidden then
                    args.Handled <- true
                    cancelQuitConfirmation ()),
            RoutingStrategies.Tunnel,
            true
        )

    override _.OnClosed(args) =
        (fsi :> IDisposable).Dispose()
        base.OnClosed(args)

type App(path: string, configuration: LfsiConfiguration) =
    inherit Application()

    override this.Initialize() = this.Styles.Add(ModernTheme())

    override _.OnFrameworkInitializationCompleted() =
        match base.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            desktop.MainWindow <- NotebookWindow(path, configuration)
        | _ -> ()

        base.OnFrameworkInitializationCompleted()

module Program =
    let private configureTerminalEnvironment () =
        NativeEnvironment.setIfMissing NativeEnvironment.EscDelayName NativeEnvironment.DefaultEscDelay

    let private buildApp path =
        configureTerminalEnvironment ()
        let configuration = LfsiConfiguration.load ()

        AppBuilder
            .Configure(fun () -> App(path, configuration))
            .UseConsolonia()
            .UseAutoDetectedConsole()
            .LogToException()

    [<EntryPoint>]
    let main argv =
        match argv |> Array.toList with
        | "--html" :: file :: _ when File.Exists file ->
            let html = LiterateScript.toHtml (Some file) (File.ReadAllText file)
            printf "%s" html
            0
        | file :: _ when File.Exists file -> (buildApp file).StartWithConsoleLifetime(argv)
        | _ ->
            eprintfn "Pass an .fsx file path."
            1
