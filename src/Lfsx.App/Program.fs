namespace Lfsx.App

open System
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
open Lfsx.Core

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

type DirtyIndicator =
    | HideDirtyIndicator
    | StarWhenDirty

type NotebookHeaderOptions = { DirtyIndicator: DirtyIndicator }

module NotebookHeader =
    let dirtyIndicatorText options isDirty =
        match options.DirtyIndicator with
        | HideDirtyIndicator -> ""
        | StarWhenDirty when isDirty -> "*"
        | StarWhenDirty -> ""

type NotebookWindow(path: string) as this =
    inherit Window(Title = "lfsx notebook", WindowState = WindowState.Maximized)

    let initialParsed, initialWriteTimeUtc = FilePersistence.load path
    let persistenceMode = AutoReloadWhenClean

    let fsiWorkingDirectory =
        let directory = Path.GetDirectoryName path

        if String.IsNullOrWhiteSpace directory then
            Environment.CurrentDirectory
        else
            directory

    let fsi = new FsiSession(fsiWorkingDirectory)

    let findFsAutocompleteDll () =
        let tryFind root =
            let storeDir = Path.Combine(root, ".dotnet/.dotnet/tools/.store/fsautocomplete")

            if Directory.Exists storeDir then
                let dlls =
                    Directory.GetFiles(storeDir, "fsautocomplete.dll", SearchOption.AllDirectories)

                dlls
                |> Array.tryFind (fun p -> p.Contains("net10.0"))
                |> Option.orElseWith (fun () -> dlls |> Array.tryHead)
            else
                None

        let rec findRoot dir =
            if
                Directory.Exists(Path.Combine(dir, ".git"))
                || File.Exists(Path.Combine(dir, "dotnet-tools.json"))
            then
                Some dir
            else
                let parent = Directory.GetParent(dir)
                if parent = null then None else findRoot parent.FullName

        let directSearches =
            [ Path.Combine(
                  Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                  ".dotnet/.dotnet/tools/.store/fsautocomplete"
              ) ]

        [ findRoot fsiWorkingDirectory
          findRoot AppContext.BaseDirectory
          findRoot Environment.CurrentDirectory ]
        |> List.choose id
        |> List.map (fun root -> Path.Combine(root, ".dotnet/.dotnet/tools/.store/fsautocomplete"))
        |> List.append directSearches
        |> List.tryFind Directory.Exists
        |> Option.map (fun storeDir ->
            let dlls =
                Directory.GetFiles(storeDir, "fsautocomplete.dll", SearchOption.AllDirectories)

            dlls
            |> Array.tryFind (fun p -> p.Contains("net10.0"))
            |> Option.orElseWith (fun () -> dlls |> Array.tryHead))
        |> Option.flatten

    let lspClient: LspClient option =
        match findFsAutocompleteDll () with
        | Some dll ->
            try
                let client = new LspClient(fsiWorkingDirectory, dll)
                client.StartAsync() |> ignore
                Some client
            with _ ->
                None
        | None -> None

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
    let mutable allCompletionItems: CompletionItem[] = [||]
    let mutable completionItems: CompletionItem[] = [||]
    let mutable showCompletionList = false
    let mutable completionSelectedIndex = 0
    let mutable completionPanel: StackPanel option = None
    let mutable completionScrollerRef: ScrollViewer option = None
    let mutable mutableSetStatus: (string -> unit) option = None
    let mutable completionFilterStart = 0

    let dismissCompletions () =
        showCompletionList <- false
        completionSelectedIndex <- 0
        completionItems <- [||]
        allCompletionItems <- [||]
        completionPanel |> Option.iter (fun panel -> panel.IsVisible <- false)
        completionScrollerRef |> Option.iter (fun s -> s.IsVisible <- false)
        mutableSetStatus |> Option.iter (fun s -> s ("Ready"))

    let updateCompletionVisual () =
        completionPanel
        |> Option.iter (fun panel ->
            for i in 0 .. panel.Children.Count - 1 do
                match panel.Children[i] with
                | :? TextBlock as tb ->
                    if i = completionSelectedIndex then
                        tb.Background <- SolidColorBrush(Color.FromRgb(60uy, 62uy, 68uy))
                        tb.Foreground <- SolidColorBrush(Colors.White)
                    else
                        tb.Background <- SolidColorBrush(Color.FromRgb(30uy, 32uy, 38uy))
                        tb.Foreground <- SolidColorBrush(Color.FromRgb(200uy, 200uy, 200uy))
                | _ -> ()

            completionScrollerRef
            |> Option.iter (fun s -> s.Offset <- Vector(s.Offset.X, float completionSelectedIndex)))

    let acceptCompletion () =
        if showCompletionList && completionSelectedIndex < completionItems.Length then
            let item = completionItems[completionSelectedIndex]
            let insertText = Option.defaultValue item.Label item.InsertText

            selectedEditor
            |> Option.iter (fun editor ->
                let caretPos = editor.CaretIndex
                let text = editor.Text
                let textBefore = text.Substring(0, caretPos)
                let textAfter = text.Substring(caretPos)
                let dotPos = textBefore.LastIndexOf('.')

                let newText =
                    if dotPos >= 0 then
                        textBefore.Substring(0, dotPos + 1) + insertText + textAfter
                    else
                        textBefore + insertText + textAfter

                let newCaret =
                    if dotPos >= 0 then
                        dotPos + 1 + insertText.Length
                    else
                        caretPos + insertText.Length

                editor.Text <- newText
                editor.CaretIndex <- newCaret)

        dismissCompletions ()

    let poplateCompletionPanel (items: CompletionItem[]) =
        allCompletionItems <- items
        completionItems <- items

        if items.Length > 0 then
            showCompletionList <- true
            completionSelectedIndex <- 0

            mutableSetStatus
            |> Option.iter (fun s -> s (sprintf "%d completions (↑↓ select, Enter accept)" items.Length))

            completionPanel
            |> Option.iter (fun panel ->
                panel.Children.Clear()

                for item in items do
                    let detail = item.Detail |> Option.map (sprintf "  (%s)") |> Option.defaultValue ""

                    panel.Children.Add(
                        TextBlock(
                            Text = item.Label + detail,
                            Foreground = SolidColorBrush(Color.FromRgb(200uy, 200uy, 200uy)),
                            Background = SolidColorBrush(Color.FromRgb(30uy, 32uy, 38uy))
                        )
                    )
                    |> ignore

                panel.IsVisible <- true

                completionScrollerRef
                |> Option.iter (fun s ->
                    s.IsVisible <- true
                    s.ScrollToHome()))
        else
            dismissCompletions ()

    let triggerCompletions (text: string) (caretIndex: int) =
        match lspClient with
        | Some client ->
            task {
                try
                    do! client.WaitForInitAsync()
                    // send didChange in the same task so the server processes it before completion
                    client.ChangeDocument(text)
                    let! items = client.RequestCompletionsAsync(text, caretIndex)
                    do! Dispatcher.UIThread.InvokeAsync(fun () -> poplateCompletionPanel items)
                with ex ->
                    do!
                        Dispatcher.UIThread.InvokeAsync(fun () ->
                            poplateCompletionPanel
                                [| { Label = "LSP error: " + ex.Message
                                     InsertText = None
                                     Detail = None
                                     Kind = None } |])
            }
            |> ignore
        | None ->
            poplateCompletionPanel
                [| { Label = "no LSP - install fsautocomplete"
                     InsertText = None
                     Detail = None
                     Kind = None }
                   { Label = "press F5 to run cells"
                     InsertText = None
                     Detail = None
                     Kind = None } |]

    let formattingStatus () =
        if parsed.FormattingDiagnostics.IsEmpty then
            "FSharp.Formatting parse: ok"
        else
            "FSharp.Formatting parse: " + String.concat "; " parsed.FormattingDiagnostics

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

    let addCellPreview
        theme
        selectedBrush
        errorBrush
        visualOutputCache
        imageBackend
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

        body.Children.Add(
            TextBlock(Text = cell.Source.TrimEnd(), Foreground = theme.Text, TextWrapping = TextWrapping.NoWrap)
        )
        |> ignore

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

    let addEditableCell
        theme
        selectedBrush
        errorBrush
        visualOutputCache
        imageBackend
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

        let mutable previousText = cell.Source

        editor.TextChanged.Add(fun _ ->
            let newText = editor.Text

            let willTrigger =
                newText.Length > previousText.Length
                && (newText[newText.Length - 1] = '.' || newText[newText.Length - 1] = '\'')

            // defer didChange to triggerCompletions when we will request completions
            if not willTrigger then
                lspClient |> Option.iter (fun client -> client.ChangeDocument(newText))

            if willTrigger then
                completionFilterStart <- newText.Length
                triggerCompletions newText (editor.CaretIndex)
            elif showCompletionList && allCompletionItems.Length > 0 then
                let filterStart = min completionFilterStart newText.Length
                let filterText = newText.Substring(filterStart).ToLowerInvariant()

                let filtered =
                    allCompletionItems
                    |> Array.filter (fun item -> item.Label.ToLowerInvariant().Contains(filterText))

                poplateCompletionPanel filtered

            previousText <- newText
            onTextChanged cell.Source newText)

        editor.AddHandler(
            InputElement.KeyDownEvent,
            (fun _ args ->
                if showCompletionList then
                    match args.Key with
                    | Key.Down ->
                        args.Handled <- true

                        completionSelectedIndex <-
                            Math.Clamp(completionSelectedIndex + 1, 0, completionItems.Length - 1)

                        updateCompletionVisual ()
                    | Key.Up ->
                        args.Handled <- true

                        completionSelectedIndex <-
                            Math.Clamp(completionSelectedIndex - 1, 0, completionItems.Length - 1)

                        updateCompletionVisual ()
                    | Key.Enter ->
                        args.Handled <- true
                        acceptCompletion ()
                    | Key.Escape ->
                        args.Handled <- true
                        dismissCompletions ()
                    | Key.Tab ->
                        args.Handled <- true
                        acceptCompletion ()
                    | _ -> ()),
            RoutingStrategies.Bubble,
            true
        )

        body.Children.Add(editor) |> ignore
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

    do
        let theme =
            { Dark = SolidColorBrush(Color.FromRgb(18uy, 18uy, 18uy))
              Panel = SolidColorBrush(Color.FromRgb(28uy, 30uy, 34uy))
              Text = SolidColorBrush(Color.FromRgb(232uy, 232uy, 232uy))
              Muted = SolidColorBrush(Color.FromRgb(170uy, 176uy, 184uy))
              Accent = SolidColorBrush(Color.FromRgb(140uy, 190uy, 255uy)) }

        let headerOptions = { DirtyIndicator = StarWhenDirty }

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
        let root = Grid(Background = theme.Dark)
        let header = DockPanel(Background = theme.Dark)

        let headerText =
            TextBlock(Foreground = theme.Accent, Background = theme.Dark, TextWrapping = TextWrapping.Wrap)

        let dirtyIndicator =
            TextBlock(Foreground = theme.Accent, Background = theme.Dark, Text = "")

        let lspText = if lspClient.IsSome then "LSP" else "noLSP"

        let lspColor =
            if lspClient.IsSome then
                SolidColorBrush(Color.FromRgb(100uy, 200uy, 100uy))
            else
                theme.Muted

        let status = DockPanel(Background = theme.Dark)

        let statusText =
            TextBlock(
                Text = formattingStatus (),
                Foreground = theme.Muted,
                Background = theme.Dark,
                TextWrapping = TextWrapping.Wrap
            )

        let lspDot =
            TextBlock(Text = lspText, Foreground = lspColor, Background = theme.Dark)

        DockPanel.SetDock(lspDot, Dock.Right)
        status.Children.Add(lspDot) |> ignore
        status.Children.Add(statusText) |> ignore

        let setStatus message = statusText.Text <- message
        mutableSetStatus <- Some setStatus

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
            setStatus (formattingStatus ())

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
            dirtyIndicator.Text <- NotebookHeader.dirtyIndicatorText headerOptions isDirty

            if cells.IsEmpty then
                headerText.Text <- "lfsx  no cells  Ctrl+C quit"
            else
                let saveHint =
                    if isDirty && FilePersistence.canSave persistenceMode then
                        "  Ctrl+S save"
                    else
                        String.Empty

                let runHint =
                    if selectedRunnableCell () |> Option.isSome then
                        "  F5 run cell"
                    else
                        String.Empty

                headerText.Text <-
                    sprintf
                        "lfsx  cell %d/%d  %s  Up/Down move  Enter edit  Esc select%s%s  Ctrl+C quit"
                        (selectedIndex + 1)
                        cells.Length
                        (modeLabel ())
                        runHint
                        saveHint

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

                // open document before rebuild so TextChanged can't race ahead
                lspClient
                |> Option.iter (fun client ->
                    let cell = cells[selectedIndex]
                    client.OpenDocument(cell.Source))

                rebuildCells ()

        let reloadFromDisk message =
            let nextParsed, nextWriteTimeUtc = FilePersistence.load path
            parsed <- nextParsed
            lastWriteTimeUtc <- nextWriteTimeUtc
            cells <- parsed.Document.Cells

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
                dismissCompletions ()
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

        let completionStack =
            StackPanel(
                Orientation = Orientation.Vertical,
                Spacing = 0.0,
                Background = SolidColorBrush(Color.FromRgb(30uy, 32uy, 38uy))
            )

        let completionScroller =
            ScrollViewer(Content = completionStack, IsVisible = false, MaxHeight = 12.0)

        completionPanel <- Some completionStack
        completionScrollerRef <- Some completionScroller

        [| GridLength.Auto; GridLength.Star; GridLength.Auto; GridLength.Auto |]
        |> Array.iter (fun h -> root.RowDefinitions.Add(RowDefinition(Height = h)))

        DockPanel.SetDock(dirtyIndicator, Dock.Right)
        header.Children.Add(dirtyIndicator) |> ignore
        header.Children.Add(headerText) |> ignore

        Grid.SetRow(header, 0)
        Grid.SetRow(scroll, 1)
        Grid.SetRow(completionScroller, 2)
        Grid.SetRow(status, 3)

        root.Children.Add(header) |> ignore
        root.Children.Add(scroll) |> ignore
        root.Children.Add(completionScroller) |> ignore
        root.Children.Add(status) |> ignore

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
                if showCompletionList then
                    match args.Key with
                    | Key.Down ->
                        args.Handled <- true

                        completionSelectedIndex <-
                            Math.Clamp(completionSelectedIndex + 1, 0, completionItems.Length - 1)

                        updateCompletionVisual ()
                    | Key.Up ->
                        args.Handled <- true

                        completionSelectedIndex <-
                            Math.Clamp(completionSelectedIndex - 1, 0, completionItems.Length - 1)

                        updateCompletionVisual ()
                    | Key.Enter ->
                        args.Handled <- true
                        acceptCompletion ()
                    | Key.Escape ->
                        args.Handled <- true
                        dismissCompletions ()
                    | Key.Tab ->
                        args.Handled <- true
                        acceptCompletion ()
                    | _ -> ()
                elif args.KeyModifiers = KeyModifiers.Control && args.Key = Key.C then
                    args.Handled <- true
                    requestQuit ()
                elif args.KeyModifiers = KeyModifiers.Control && args.Key = Key.S then
                    args.Handled <- true
                    saveToDisk ()
                elif args.KeyModifiers = KeyModifiers.Control && args.Key = Key.Space then
                    args.Handled <- true

                    if isEditing then
                        match lspClient with
                        | Some _ ->
                            selectedEditor
                            |> Option.iter (fun editor -> triggerCompletions editor.Text editor.CaretIndex)
                        | None ->
                            poplateCompletionPanel
                                [| { Label = "test1 (no LSP)"
                                     InsertText = None
                                     Detail = None
                                     Kind = None }
                                   { Label = "test2 (no LSP)"
                                     InsertText = None
                                     Detail = None
                                     Kind = None } |]
                elif args.Key = Key.F5 && (selectedRunnableCell () |> Option.isSome) then
                    args.Handled <- true
                    runSelectedAsync () |> ignore
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

        match lspClient with
        | Some client -> (client :> IDisposable).Dispose()
        | None -> ()

        base.OnClosed(args)

type App(path: string) =
    inherit Application()

    override this.Initialize() = this.Styles.Add(ModernTheme())

    override _.OnFrameworkInitializationCompleted() =
        match base.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.MainWindow <- NotebookWindow(path)
        | _ -> ()

        base.OnFrameworkInitializationCompleted()

module Program =
    let private configureTerminalEnvironment () =
        NativeEnvironment.setIfMissing NativeEnvironment.EscDelayName NativeEnvironment.DefaultEscDelay

    let private buildApp path =
        configureTerminalEnvironment ()

        AppBuilder.Configure(fun () -> App(path)).UseConsolonia().UseAutoDetectedConsole().LogToException()

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
