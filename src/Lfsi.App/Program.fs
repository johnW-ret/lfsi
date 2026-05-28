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

    [<Literal>]
    let private StdOutputHandle = -11

    [<Literal>]
    let private EnableVirtualTerminalProcessing = 0x0004u

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern nativeint GetStdHandle(int nStdHandle)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool GetConsoleMode(nativeint hConsoleHandle, uint32& lpMode)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool SetConsoleMode(nativeint hConsoleHandle, uint32 dwMode)

    // .NET environment mutation is not visible to native getenv on Unix for some reason
    let setIfMissing name value =
        if
            not (OperatingSystem.IsWindows())
            && (Environment.GetEnvironmentVariable(name) |> String.IsNullOrWhiteSpace)
        then
            setenv (name, value, 1) |> ignore

    let enableVirtualTerminalOutput () =
        if OperatingSystem.IsWindows() then
            let handle = GetStdHandle StdOutputHandle
            let mutable mode = 0u

            if handle <> nativeint -1 && GetConsoleMode(handle, &mode) then
                SetConsoleMode(handle, mode ||| EnableVirtualTerminalProcessing) |> ignore

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

    let terminalGraphicsDecision =
        TerminalGraphics.currentEnvironment () |> TerminalGraphics.decide

    let richDisplayEnabled =
        match terminalGraphicsDecision with
        | UseTerminalGraphics Kitty
        | UseTerminalGraphics Sixel -> true
        | UseTerminalGraphics _
        | UseTextFallback _ -> false

    let fsi =
        new FsiSession(fsiWorkingDirectory, configuration.Fsi.ExecutablePath, richDisplayEnabled)

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

        let visualOutputService =
            if richDisplayEnabled then
                ChromeCdpVisualOutputService() :> IVisualOutputService
            else
                FallbackVisualOutputService() :> IVisualOutputService

        let visualOutputCache = MemoryVisualOutputCache visualOutputService

        let imageBackend, terminalImageLayer =
            match terminalGraphicsDecision with
            | UseTerminalGraphics Kitty ->
                let backend = KittyImageBackend()
                backend :> ITerminalImageBackend, backend :> ITerminalImageLayer
            | UseTerminalGraphics Sixel ->
                let backend = SixelImageBackend()
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

        scroll.ScrollChanged.Add(fun _ -> cellStack.InvalidateVisual())




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
        NativeEnvironment.enableVirtualTerminalOutput ()

    let private buildApp path =
        configureTerminalEnvironment ()
        let configuration = LfsiConfiguration.load ()

        let builder =
            AppBuilder
                .Configure(fun () -> App(path, configuration))
                .UseConsolonia()

        let builder =
            match TerminalGraphics.currentEnvironment () |> TerminalGraphics.decide with
            | UseTerminalGraphics Sixel ->
                builder
                    .UseAutoDetectConsoleColorMode()
                    .UseAutoDetectedConsole()
            | _ -> builder.UseAutoDetectedConsole()

        builder.LogToException()

    let private sixelSmoke () =
        configureTerminalEnvironment ()
        let backend = SixelImageBackend()
        let width = 32
        let height = 18

        let pixels =
            [| for y in 0 .. height - 1 do
                   for x in 0 .. width - 1 do
                       let red = x < width / 2
                       yield 0uy
                       yield (if red then 0uy else 255uy)
                       yield (if red then 255uy else 0uy)
                       yield 255uy |]

        printfn "If Sixel is supported, a red/green block should appear below:"
        Console.Write(backend.DiagnosticSixelSequence(width, height, pixels))
        Console.Out.Flush()
        printfn ""
        0

    let private sixelCanvasSmoke () =
        configureTerminalEnvironment ()
        let backend = SixelImageBackend()
        let width = 640
        let height = 360
        let canvasColumn = 3
        let canvasRow = 5
        let reservedRows = 18

        let pixels =
            [| for y in 0 .. height - 1 do
                   for x in 0 .. width - 1 do
                       let grid = x % 80 = 0 || y % 60 = 0
                       let axis = x = 56 || y = height - 48
                       let lineY =
                           let t = float x / float (width - 1)
                           int (float (height - 72) - (sin (t * Math.PI * 3.0) * 80.0 + t * 140.0))

                       let marker = abs (y - lineY) <= 3

                       let r, g, b =
                           if marker then
                               255uy, 210uy, 64uy
                           elif axis then
                               220uy, 225uy, 235uy
                           elif grid then
                               52uy, 58uy, 76uy
                           else
                               12uy, 14uy, 22uy

                       yield b
                       yield g
                       yield r
                       yield 255uy |]

        let sixel = backend.DiagnosticSixelSequence(width, height, pixels)

        let writeAt row column text =
            Console.Write(sprintf "\u001b[%d;%dH%s" row column text)

        try
            Console.Write("\u001b[?1049h\u001b[?25l\u001b[2J\u001b[H")
            writeAt 1 1 "lfsx sixel canvas smoke"
            writeAt 2 1 "Expected: a wide dark chart with grid lines and a yellow curve, inside the reserved canvas."
            writeAt 3 1 "Press any key to return."

            for row in 0 .. reservedRows - 1 do
                writeAt (canvasRow + row) canvasColumn (String(' ', 80))

            writeAt canvasRow canvasColumn ""
            Console.Write(sixel)
            Console.Out.Flush()
            Console.ReadKey(true) |> ignore
            0
        finally
            Console.Write("\u001b[?25h\u001b[?1049l")
            Console.Out.Flush()


    /// Capture mode: write all escape sequences to a log file for analysis
    let private sixelCaptureDiag () =
        configureTerminalEnvironment ()
        let capturePath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "sixel-capture.bin")
        let logPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "sixel-capture.log")
        let log = new StreamWriter(logPath, false, System.Text.Encoding.UTF8)
        log.AutoFlush <- true

        let backend = SixelImageBackend()
        let width = 200
        let height = 120
        let pixels =
            [| for y in 0 .. height - 1 do
                   for x in 0 .. width - 1 do
                       let r = byte (x * 255 / width)
                       let g = byte (y * 255 / height)
                       let b = 128uy
                       yield b; yield g; yield r; yield 255uy |]

        let sixel = backend.DiagnosticSixelSequence(width, height, pixels)
        log.WriteLine(sprintf "Sixel data length: %d chars" sixel.Length)

        // Build the EXACT sequence that the Consolonia + our control would send
        let totalOutput = System.Text.StringBuilder()

        // === Phase 1: Consolonia PrepareConsole ===
        totalOutput.Append("\u001b[?1049h") |> ignore // EnableAlternateBuffer
        // Consolonia does emoji test here but we skip it
        // BlackColorTTYWorkaround:
        totalOutput.Append("\u001b[36m\u001b[46m \u001b[30m\u001b[40m") |> ignore
        // ClearScreen:
        totalOutput.Append("\u001b[2J\u001b[1;1f") |> ignore

        // === Phase 2: Consolonia RenderToDevice ===
        // HideCaret (note: in real Consolonia this is a separate Flush)
        totalOutput.Append("\u001b[?25l") |> ignore

        // Write cells across the screen (simplified - just the canvas area)
        let canvasRow = 5
        let canvasCol = 3
        let canvasWidth = 30
        let reservedRows = 8

        // Write some text above
        totalOutput.Append(sprintf "\u001b[1;1f") |> ignore
        totalOutput.Append("\u001b[38;2;220;220;220m\u001b[48;2;30;30;40m") |> ignore
        totalOutput.Append("Consolonia simulation output") |> ignore

        // Write blank cells in the canvas area (like Consolonia rendering the RawSixelImageControl background)
        for row in 0 .. reservedRows - 1 do
            for col in 0 .. canvasWidth - 1 do
                totalOutput.Append(sprintf "\u001b[%d;%df" (canvasRow + row) (canvasCol + col)) |> ignore
                totalOutput.Append("\u001b[48;2;25;25;35m\u001b[38;2;25;25;35m") |> ignore
                totalOutput.Append(' ') |> ignore

        // Consolonia Flush (single Console.Write)
        // Then ShowCaret or HideCaret at end of render
        totalOutput.Append("\u001b[?25l") |> ignore // HideCaret again (as in real Consolonia)

        log.WriteLine(sprintf "Consolonia render buffer: %d chars" totalOutput.Length)

        // === Phase 3: Our RawSixelImageControl.emit (fires at Background priority AFTER render) ===
        let sixelWrite = sprintf "\u001b7\u001b[%d;%dH%s\u001b8" canvasRow canvasCol sixel
        log.WriteLine(sprintf "Sixel write: %d chars" sixelWrite.Length)
        log.WriteLine(sprintf "Sixel write prefix: %s" (sixelWrite.Substring(0, Math.Min(120, sixelWrite.Length)).Replace("\u001b", "<ESC>")))

        // Append sixel write to total output (in reality this is a separate Console.Write call)
        totalOutput.Append(sixelWrite) |> ignore

        log.WriteLine(sprintf "Total output: %d chars" totalOutput.Length)

        // Write the binary capture
        let outputStr = totalOutput.ToString()
        File.WriteAllText(capturePath, outputStr, System.Text.Encoding.UTF8)
        log.WriteLine(sprintf "Wrote capture to: %s" capturePath)

        // Analyze the sequence for potential issues
        log.WriteLine("")
        log.WriteLine("=== ANALYSIS ===")

        // Check: does the sixel DCS start correctly?
        let sixelStart = outputStr.IndexOf("\u001bPq")
        let sixelEnd = outputStr.IndexOf("\u001b\\", sixelStart + 1)
        log.WriteLine(sprintf "Sixel DCS starts at offset: %d" sixelStart)
        log.WriteLine(sprintf "Sixel DCS ends at offset: %d (ST)" sixelEnd)
        log.WriteLine(sprintf "Total output length: %d" outputStr.Length)

        // Check: what's immediately before the sixel DCS?
        if sixelStart > 0 then
            let before = outputStr.Substring(Math.Max(0, sixelStart - 30), Math.Min(30, sixelStart))
            log.WriteLine(sprintf "30 chars before DCS: %s" (before.Replace("\u001b", "<ESC>")))

        // Check: what's after the sixel DCS?
        if sixelEnd >= 0 && sixelEnd + 2 < outputStr.Length then
            let after = outputStr.Substring(sixelEnd, Math.Min(30, outputStr.Length - sixelEnd))
            log.WriteLine(sprintf "30 chars after ST: %s" (after.Replace("\u001b", "<ESC>")))

        // Check: are there any escape sequences INSIDE the sixel DCS that shouldn't be there?
        let sixelBody = outputStr.Substring(sixelStart + 3, sixelEnd - sixelStart - 3)
        let escInSixel = sixelBody.Split('\u001b').Length - 1
        log.WriteLine(sprintf "ESC characters inside sixel body: %d (should be 0)" escInSixel)

        // Check: the raster attributes
        let rasterAttrStart = sixelBody.IndexOf('"')
        if rasterAttrStart >= 0 then
            let rasterAttrEnd = sixelBody.IndexOf('#', rasterAttrStart)
            if rasterAttrEnd > rasterAttrStart then
                let rasterAttrs = sixelBody.Substring(rasterAttrStart, rasterAttrEnd - rasterAttrStart)
                log.WriteLine(sprintf "Raster attributes: %s" rasterAttrs)

        log.WriteLine("")
        log.WriteLine("=== DONE ===")
        log.Close()
        eprintfn "Capture written to: %s" capturePath
        eprintfn "Log written to: %s" logPath
        0

    /// Diagnostic: simulates the Consolonia terminal state and tests sixel rendering
    /// in multiple configurations to isolate why sixel breaks inside Consolonia.
    let private sixelConsoloniaDiag () =
        configureTerminalEnvironment ()
        let logPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "sixel-diag.log")
        let log = new StreamWriter(logPath, false, System.Text.Encoding.UTF8)
        log.AutoFlush <- true
        log.WriteLine("=== sixel-consolonia-diag started ===")
        log.WriteLine(sprintf "Terminal: %s" (Environment.GetEnvironmentVariable("TERM_PROGRAM") |> Option.ofObj |> Option.defaultValue "unknown"))
        log.WriteLine(sprintf "WT_SESSION: %s" (Environment.GetEnvironmentVariable("WT_SESSION") |> Option.ofObj |> Option.defaultValue "not set"))
        try log.WriteLine(sprintf "Console size: %dx%d" Console.WindowWidth Console.WindowHeight) with _ -> log.WriteLine("Console size: unavailable")

        let backend = SixelImageBackend()
        let width = 200
        let height = 120
        let pixels =
            [| for y in 0 .. height - 1 do
                   for x in 0 .. width - 1 do
                       let r = byte (x * 255 / width)
                       let g = byte (y * 255 / height)
                       let b = 128uy
                       yield b; yield g; yield r; yield 255uy |]

        let sixel = backend.DiagnosticSixelSequence(width, height, pixels)
        log.WriteLine(sprintf "Sixel data length: %d bytes" sixel.Length)
        log.WriteLine(sprintf "Sixel starts with: %s" (sixel.Substring(0, Math.Min(80, sixel.Length)).Replace("\u001b", "<ESC>")))
        log.WriteLine(sprintf "Sixel ends with: %s" (sixel.Substring(Math.Max(0, sixel.Length - 20)).Replace("\u001b", "<ESC>")))

        let writeAt row col (text: string) =
            Console.Write(sprintf "\u001b[%d;%dH%s" row col text)

        let canvasRow = 5
        let canvasCol = 3
        let reservedRows = 8
        let canvasWidth = 30 // approximate cell width for the image

        try
            // ===== TEST A: Baseline sixel (like smoke test) =====
            Console.Write("\u001b[?1049h\u001b[?25l\u001b[2J\u001b[H")
            writeAt 1 1 "TEST A: Baseline sixel (same as smoke test)"
            writeAt 2 1 "Expected: gradient rectangle at row 5, col 3"
            writeAt 3 1 "[Press any key for next test]"
            // Position cursor and write sixel directly
            writeAt canvasRow canvasCol ""
            Console.Write(sixel)
            Console.Out.Flush()
            log.WriteLine("TEST A: Wrote sixel directly after CUP. No SGR state.")
            Console.ReadKey(true) |> ignore

            // ===== TEST B: SGR state before sixel =====
            Console.Write("\u001b[2J\u001b[H")
            writeAt 1 1 "TEST B: Set SGR fg/bg colors, then write sixel"
            writeAt 2 1 "Expected: same gradient rectangle (SGR should not affect sixel)"
            writeAt 3 1 "[Press any key for next test]"
            // Set some SGR state like Consolonia would
            Console.Write("\u001b[38;2;255;0;0m")  // red foreground
            Console.Write("\u001b[48;2;0;0;255m")  // blue background
            Console.Write("\u001b[1m")              // bold
            // Now position and write sixel
            writeAt canvasRow canvasCol ""
            Console.Write(sixel)
            Console.Out.Flush()
            Console.Write("\u001b[0m")  // reset SGR
            log.WriteLine("TEST B: Set SGR (red fg, blue bg, bold), then wrote sixel.")
            Console.ReadKey(true) |> ignore

            // ===== TEST C: Fill area with colored cells (like Consolonia), then sixel with DECSC/DECRC =====
            Console.Write("\u001b[2J\u001b[H")
            writeAt 1 1 "TEST C: Fill canvas area with colored cells (like Consolonia render), then sixel with DECSC/DECRC"
            writeAt 2 1 "Expected: gradient rectangle overlaying the colored cells"
            writeAt 3 1 "[Press any key for next test]"
            // Simulate Consolonia writing colored cells to the canvas area
            for row in 0 .. reservedRows - 1 do
                Console.Write(sprintf "\u001b[%d;%df" (canvasRow + row) canvasCol) // CUP using 'f' like Consolonia
                Console.Write("\u001b[38;2;200;200;200m")  // light gray foreground
                Console.Write("\u001b[48;2;30;30;50m")     // dark blue background
                Console.Write(String('.', canvasWidth))
            Console.Out.Flush()
            log.WriteLine("TEST C: Filled canvas area with SGR-colored cells using 'f' cursor positioning.")
            // Now write sixel with DECSC/DECRC (like our RawSixelImageControl)
            Console.Write(sprintf "\u001b7\u001b[%d;%dH%s\u001b8" canvasRow canvasCol sixel)
            Console.Out.Flush()
            log.WriteLine("TEST C: Wrote sixel with DECSC/DECRC wrapping.")
            Console.ReadKey(true) |> ignore

            // ===== TEST D: Fill area, sixel, then fill area again (simulating render loop overwrite) =====
            Console.Write("\u001b[2J\u001b[H")
            writeAt 1 1 "TEST D: Fill area + sixel + fill area again (simulates Consolonia re-render)"
            writeAt 2 1 "Expected: colored dots OVER the sixel (sixel should be destroyed)"
            writeAt 3 1 "[Press any key for next test]"
            // Fill area
            for row in 0 .. reservedRows - 1 do
                Console.Write(sprintf "\u001b[%d;%df" (canvasRow + row) canvasCol)
                Console.Write("\u001b[48;2;30;30;50m")
                Console.Write(String(' ', canvasWidth))
            Console.Out.Flush()
            // Write sixel
            Console.Write(sprintf "\u001b7\u001b[%d;%dH%s\u001b8" canvasRow canvasCol sixel)
            Console.Out.Flush()
            // Now simulate Consolonia re-rendering: write cells over the sixel area
            for row in 0 .. reservedRows - 1 do
                Console.Write(sprintf "\u001b[%d;%df" (canvasRow + row) canvasCol)
                Console.Write("\u001b[38;2;255;255;0m")
                Console.Write("\u001b[48;2;30;30;50m")
                Console.Write(String('X', canvasWidth))
            Console.Out.Flush()
            log.WriteLine("TEST D: Fill + Sixel + Fill again. Sixel should be overwritten.")
            Console.ReadKey(true) |> ignore

            // ===== TEST E: Fill area, then sixel WITHOUT DECSC/DECRC =====
            Console.Write("\u001b[2J\u001b[H")
            writeAt 1 1 "TEST E: Fill area with Consolonia-style cells, then sixel WITHOUT save/restore"
            writeAt 2 1 "Expected: gradient rectangle. Check if cursor position is wrong after."
            writeAt 3 1 "[Press any key for next test]"
            for row in 0 .. reservedRows - 1 do
                Console.Write(sprintf "\u001b[%d;%df" (canvasRow + row) canvasCol)
                Console.Write("\u001b[48;2;30;30;50m")
                Console.Write(String(' ', canvasWidth))
            Console.Out.Flush()
            // Reset SGR before sixel
            Console.Write("\u001b[0m")
            writeAt canvasRow canvasCol ""
            Console.Write(sixel)
            Console.Out.Flush()
            // Now write text AFTER sixel to see where cursor ended up
            Console.Write("\u001b[0m")
            Console.Write(" <-- cursor landed here after sixel")
            Console.Out.Flush()
            log.WriteLine("TEST E: Wrote sixel without DECSC/DECRC to see cursor position after.")
            Console.ReadKey(true) |> ignore

            // ===== TEST F: Consolonia-exact simulation with buffered output =====
            Console.Write("\u001b[2J\u001b[H")
            writeAt 1 1 "TEST F: Exact Consolonia simulation (buffered render + separate sixel write)"
            writeAt 2 1 "Expected: gradient rectangle at canvas position"
            writeAt 3 1 "[Press any key to exit]"
            // Simulate Consolonia's RenderToDevice: build a buffer of escape sequences
            let buf = System.Text.StringBuilder()
            buf.Append("\u001b[?25l") |> ignore  // HideCaret (with flush in real Consolonia)
            // Write cells in the canvas area (like Consolonia would for blank reserved rows)
            for row in 0 .. reservedRows - 1 do
                for col in 0 .. canvasWidth - 1 do
                    // SetCaretPosition (Consolonia uses 'f' for HVP)
                    buf.Append(sprintf "\u001b[%d;%df" (canvasRow + row) (canvasCol + col)) |> ignore
                    // Background color
                    buf.Append("\u001b[48;2;25;25;35m") |> ignore
                    // Foreground color
                    buf.Append("\u001b[38;2;25;25;35m") |> ignore
                    // Write space character
                    buf.Append(' ') |> ignore
            // Flush the entire buffer at once (like Consolonia)
            Console.Write(buf.ToString())
            log.WriteLine(sprintf "TEST F: Consolonia buffer size: %d chars" (buf.Length))
            // Now write sixel (like our RawSixelImageControl.emit at Background priority)
            Console.Write(sprintf "\u001b7\u001b[%d;%dH%s\u001b8" canvasRow canvasCol sixel)
            Console.Out.Flush()
            log.WriteLine("TEST F: Wrote sixel after Consolonia-style buffered render.")
            Console.ReadKey(true) |> ignore

            log.WriteLine("=== All tests completed ===")
            log.Close()
            0
        finally
            Console.Write("\u001b[0m\u001b[?25h\u001b[?1049l")
            Console.Out.Flush()

    [<EntryPoint>]
    let main argv =
        match argv |> Array.toList with
        | "--sixel-smoke" :: _ -> sixelSmoke ()
        | "--sixel-canvas-smoke" :: _ -> sixelCanvasSmoke ()
        | "--sixel-consolonia-diag" :: _ -> sixelConsoloniaDiag ()
        | "--sixel-capture-diag" :: _ -> sixelCaptureDiag ()
        | "--html" :: file :: _ when File.Exists file ->
            let html = LiterateScript.toHtml (Some file) (File.ReadAllText file)
            printf "%s" html
            0
        | file :: _ when File.Exists file -> (buildApp file).StartWithConsoleLifetime(argv)
        | _ ->
            eprintfn "Pass an .fsx file path."
            1