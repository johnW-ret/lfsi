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

type HeaderKey =
    | UpDown
    | Enter
    | A
    | B
    | XCV
    | MY
    | Esc
    | F5
    | F6
    | CtrlS
    | CtrlC

type HeaderLabel =
    | Move
    | Edit
    | Above
    | Below
    | CutCopyPaste
    | MarkdownCode
    | Select
    | Run
    | RunAll
    | Save
    | Quit

type HeaderAction = { Key: HeaderKey; Label: HeaderLabel }

type HeaderModel =
    { AppName: string
      CellPosition: string option
      Mode: string
      IsDirty: bool
      Actions: HeaderAction list }

type WordJumpDirection =
    | PreviousWord
    | NextWord

type NotebookMode =
    | Selecting
    | Editing of TextBox option
    | Running

type CellSelection =
    | NoCells
    | Selected of index: int

type DocumentChangeState =
    | Clean
    | Dirty
    | ExternalChanged
    | DirtyWithExternalChanged

type NotebookUiState =
    { Mode: NotebookMode
      DocumentChange: DocumentChangeState
      QuitConfirmation: QuitConfirmation }

type SavePrompt =
    | SavePromptInactive
    | SavePromptActive of TextBox

module NotebookHeader =
    let private renderKey key =
        match key with
        | UpDown -> "↑↓"
        | Enter -> "Enter"
        | A -> "A"
        | B -> "B"
        | XCV -> "X/C/V"
        | MY -> "M/Y"
        | Esc -> "Esc"
        | F5 -> "F5"
        | F6 -> "F6"
        | CtrlS -> "Ctrl+S"
        | CtrlC -> "Ctrl+C"

    let private renderLabel label =
        match label with
        | Move -> "move"
        | Edit -> "edit"
        | Above -> "above"
        | Below -> "below"
        | CutCopyPaste -> "cut/copy/paste"
        | MarkdownCode -> "markdown/code"
        | Select -> "select"
        | Run -> "run"
        | RunAll -> "run all"
        | Save -> "save"
        | Quit -> "quit"

    let private renderAction action =
        renderKey action.Key + " " + renderLabel action.Label

    let render model =
        let position =
            model.CellPosition
            |> Option.map (fun value -> "  " + value)
            |> Option.defaultValue ""

        let dirty = if model.IsDirty then " *" else ""

        let actions = model.Actions |> List.map renderAction |> String.concat "  "

        model.AppName + position + dirty + "  " + model.Mode + "  " + actions

type NotebookWindow(initialPath: string option, configuration: LfsiConfiguration) as this =
    inherit Window(Title = "lfsi notebook", WindowState = WindowState.Maximized)

    let initialParsed, initialWriteTimeUtc =
        initialPath
        |> Option.map FilePersistence.load
        |> Option.defaultWith FilePersistence.newDocument

    let persistenceMode = AutoReloadWhenClean

    let fsiWorkingDirectory =
        initialPath
        |> Option.map Path.GetDirectoryName
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        |> Option.defaultValue Environment.CurrentDirectory

    let terminalGraphicsDecision =
        TerminalGraphics.currentEnvironment () |> TerminalGraphics.decide

    let richDisplayEnabled =
        match terminalGraphicsDecision with
        | UseTerminalGraphics Kitty
        | UseTerminalGraphics Sixel -> true
        | UseTerminalGraphics _
        | UseTextFallback _ -> false

    let fsi =
        new FsiSession(
            fsiWorkingDirectory,
            LfsiConfiguration.fsiExecutablePath configuration.Fsi.Executable,
            richDisplayEnabled
        )

    let completionService: ICompletionService =
        if configuration.FsAutoComplete.Enabled then
            new FsAutoCompleteCompletionService(fsiWorkingDirectory, initialPath)
        else
            new DisabledCompletionService()

    let quitConfirmationMessage = "Press Ctrl+C again to quit, or Esc to cancel."
    let mutable parsed = initialParsed
    let mutable lastWriteTimeUtc = initialWriteTimeUtc
    let mutable documentPath = initialPath
    let mutable cells = parsed.Document.Cells

    let mutable selection = if cells.IsEmpty then NoCells else Selected 0

    let mutable uiState =
        { Mode = Selecting
          DocumentChange = Clean
          QuitConfirmation = Hidden }

    let mutable cellClipboard: NotebookCell option = None

    let selectedIndex () =
        match selection with
        | NoCells -> None
        | Selected index -> Some index

    let selectedIndexOrZero () =
        selectedIndex () |> Option.defaultValue 0

    let selectedCell () =
        selectedIndex () |> Option.bind (fun index -> cells |> List.tryItem index)

    let selectIndex index =
        selection <-
            if cells.IsEmpty then
                NoCells
            else
                Selected(Math.Clamp(index, 0, cells.Length - 1))

    let isEditing () =
        match uiState.Mode with
        | Editing _ -> true
        | Selecting
        | Running -> false

    let isRunning () =
        match uiState.Mode with
        | Running -> true
        | Selecting
        | Editing _ -> false

    let selectedEditor () =
        match uiState.Mode with
        | Editing editor -> editor
        | Selecting
        | Running -> None

    let withMode mode = uiState <- { uiState with Mode = mode }

    let withSelectedEditor editor =
        uiState <- { uiState with Mode = Editing editor }

    let isDirty () =
        match uiState.DocumentChange with
        | Dirty
        | DirtyWithExternalChanged -> true
        | Clean
        | ExternalChanged -> false

    let hasExternalChanges () =
        match uiState.DocumentChange with
        | ExternalChanged
        | DirtyWithExternalChanged -> true
        | Clean
        | Dirty -> false

    let markDocumentDirty () =
        uiState <-
            { uiState with
                DocumentChange =
                    match uiState.DocumentChange with
                    | Clean
                    | Dirty -> Dirty
                    | ExternalChanged
                    | DirtyWithExternalChanged -> DirtyWithExternalChanged }

    let markExternalChanged () =
        uiState <-
            { uiState with
                DocumentChange =
                    match uiState.DocumentChange with
                    | Clean
                    | ExternalChanged -> ExternalChanged
                    | Dirty
                    | DirtyWithExternalChanged -> DirtyWithExternalChanged }

    let markDocumentClean () =
        uiState <- { uiState with DocumentChange = Clean }

    let withQuitConfirmation quitConfirmation =
        uiState <-
            { uiState with
                QuitConfirmation = quitConfirmation }

    let restoreStatus (status: TextBlock) = status.Text <- "Ready"

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
        | CellKind.Markdown -> MarkdownRendering.render theme (cell.Source.TrimEnd())

    let addCellPreview
        theme
        selectedBrush
        errorBrush
        visualOutputCache
        imageBackend
        renderCellSource
        selection
        (cellStack: StackPanel)
        index
        cell
        =
        let isSelected =
            match selection with
            | Selected selectedIndex -> index = selectedIndex
            | NoCells -> false

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
        (completionService: ICompletionService)
        (getCompletionContext: string -> int -> CompletionContext)
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

        let completionPopup =
            match cell.Kind with
            | CellKind.Code -> Some(EditorCompletion.attach theme editor completionService getCompletionContext)
            | CellKind.Markdown -> None

        editor.TextChanged.Add(fun _ ->
            let editorText = editor.Text |> Option.ofObj |> Option.defaultValue ""

            onTextChanged cell.Source editorText

            highlightedEditor
            |> Option.iter (fun block -> updateHighlightedCode editorText block))

        editor.AddHandler(
            InputElement.KeyDownEvent,
            (fun _ args ->
                if not args.Handled then
                    match tryGetWordJumpDirection args with
                    | Some direction ->
                        args.Handled <- true
                        moveEditorCaretByWord direction editor
                    | None -> ()),
            RoutingStrategies.Tunnel,
            true
        )

        body.Children.Add(editor) |> ignore
        completionPopup |> Option.iter (fun popup -> body.Children.Add popup |> ignore)

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
        |> List.mapi (fun index (cell: NotebookCell) ->
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

        let completionContext selectedIndex editorText caretIndex =
            let mutable cursorOffset = 0

            let sources =
                cells
                |> List.indexed
                |> List.choose (fun (index, notebookCell) ->
                    if notebookCell.Kind <> CellKind.Code then
                        None
                    else
                        let source =
                            if index = selectedIndex then
                                editorText
                            else
                                notebookCell.Source

                        if index = selectedIndex then
                            cursorOffset <- cursorOffset + caretIndex
                        elif index < selectedIndex then
                            cursorOffset <- cursorOffset + source.Length + 2

                        Some source)

            { Source = String.concat "\n\n" sources
              CursorOffset = cursorOffset }

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
                Text = "Ready",
                Foreground = theme.Muted,
                Background = theme.Dark,
                TextWrapping = TextWrapping.Wrap
            )

        let footer = ContentControl(Content = status)
        let mutable savePrompt = SavePromptInactive

        let setStatus message = status.Text <- message

        let showStatus () =
            savePrompt <- SavePromptInactive
            footer.Content <- status

        let quit () =
            match Application.Current.ApplicationLifetime with
            | :? IControlledApplicationLifetime as lifetime -> lifetime.Shutdown()
            | _ -> this.Close()

        let requestQuit () =
            match uiState.QuitConfirmation with
            | Hidden ->
                withQuitConfirmation Arming
                setStatus quitConfirmationMessage

                Task
                    .Delay(1)
                    .ContinueWith(fun _ ->
                        if uiState.QuitConfirmation = Arming then
                            withQuitConfirmation Armed)
                |> ignore
            | Arming -> setStatus quitConfirmationMessage
            | Armed -> quit ()

        let cancelQuitConfirmation () =
            withQuitConfirmation Hidden
            restoreStatus status

        let applySelectedEdit () =
            selectedEditor ()
            |> Option.iter (fun editor ->
                let currentSource = selectedCell () |> Option.map _.Source |> Option.defaultValue ""

                if editor.Text <> currentSource then
                    selectedIndex ()
                    |> Option.iter (fun index ->
                        cells <- cells |> replaceCellSource index editor.Text
                        markDocumentDirty ()))

        let cellStack = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)
        let mutable selectedFrame: Control option = None

        let clearTerminalImages () = terminalImageLayer.Clear()

        let modeLabel () =
            match uiState.Mode with
            | Running -> "running"
            | Editing _ -> "editing"
            | Selecting -> "selection"

        let selectedRunnableCell () =
            selectedIndex ()
            |> Option.bind (fun index ->
                cells
                |> List.tryItem index
                |> Option.filter (fun cell -> cell.Kind = CellKind.Code)
                |> Option.map (fun cell -> index, cell))

        let runnableCells () =
            cells
            |> List.indexed
            |> List.choose (fun (index, cell) ->
                if cell.Kind = CellKind.Code then
                    Some(index, cell)
                else
                    None)

        let isSelectedEditor index =
            isEditing () && selection = Selected index

        let updateHeader () =
            let actions =
                [ if not (isEditing ()) then
                      { Key = UpDown; Label = Move }
                      { Key = Enter; Label = Edit }
                      { Key = A; Label = Above }
                      { Key = B; Label = Below }
                      { Key = XCV; Label = CutCopyPaste }
                      { Key = MY; Label = MarkdownCode }

                  if isEditing () then
                      { Key = Esc; Label = Select }

                  if selectedRunnableCell () |> Option.isSome then
                      { Key = F5; Label = Run }

                  if runnableCells () |> List.isEmpty |> not then
                      { Key = F6; Label = RunAll }

                  if isDirty () then
                      { Key = CtrlS; Label = Save }

                  { Key = CtrlC; Label = Quit } ]

            let cellPosition =
                match selection with
                | NoCells -> Some "no cells"
                | Selected selectedIndex -> Some(sprintf "%d/%d" (selectedIndex + 1) cells.Length)

            let mode =
                match selection with
                | NoCells -> "selection"
                | Selected _ -> modeLabel ()

            headerText.Text <-
                NotebookHeader.render
                    { AppName = "lfsi"
                      CellPosition = cellPosition
                      Mode = mode
                      IsDirty = isDirty ()
                      Actions = actions }

        let markDirty originalSource editedSource =
            if editedSource <> originalSource && not (isDirty ()) then
                markDocumentDirty ()
                setStatus "Unsaved in-memory edits."
                updateHeader ()

        let rebuildCells () =
            clearTerminalImages ()

            if isEditing () then
                withSelectedEditor None

            selectedFrame <- None
            cellStack.Children.Clear()

            cells
            |> List.iteri (fun index cell ->
                if isSelectedEditor index then
                    withSelectedEditor (
                        Some(
                            addEditableCell
                                theme
                                selectedBrush
                                errorBrush
                                visualOutputCache
                                imageBackend
                                completionService
                                (completionContext index)
                                renderCodeSource
                                updateHighlightedCode
                                cellStack
                                markDirty
                                index
                                cell
                        )
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
                            selection
                            cellStack
                            index
                            cell

                    if selection = Selected index then
                        selectedFrame <- Some frame)

            updateHeader ()

            selectedEditor ()
            |> Option.iter (fun editor ->
                Dispatcher.UIThread.Post(fun () ->
                    editor.Focus() |> ignore
                    editor.CaretIndex <- editor.Text.Length))

            selectedFrame
            |> Option.iter (fun frame -> Dispatcher.UIThread.Post(fun () -> frame.BringIntoView()))

        let moveSelection delta =
            match selection with
            | Selected currentIndex when not (isEditing ()) ->
                let last = cells.Length - 1
                let next = Math.Clamp(currentIndex + delta, 0, last)

                if next <> currentIndex then
                    selection <- Selected next
                    rebuildCells ()
            | NoCells
            | Selected _ -> ()

        let beginEditing () =
            if (selectedCell ()).IsSome && not (isEditing ()) && not (isRunning ()) then
                withMode (Editing None)
                rebuildCells ()

        let addCellBelow () =
            if not (isEditing ()) && not (isRunning ()) then
                let insertIndex = selectedIndex () |> Option.map ((+) 1) |> Option.defaultValue 0

                cells <- cells |> insertCellAt insertIndex (NotebookCell.create CellKind.Code "")
                selectIndex insertIndex
                markDocumentDirty ()
                setStatus "Added code cell below."
                rebuildCells ()

        let addCellAbove () =
            if not (isEditing ()) && not (isRunning ()) then
                let insertIndex = selectedIndexOrZero ()

                cells <- cells |> insertCellAt insertIndex (NotebookCell.create CellKind.Code "")
                selectIndex insertIndex
                markDocumentDirty ()
                setStatus "Added code cell above."
                rebuildCells ()

        let copyCell () =
            if not (isEditing ()) && not (isRunning ()) then
                selectedCell ()
                |> Option.iter (fun cell ->
                    cellClipboard <- Some cell
                    setStatus "Copied cell.")

        let cutCell () =
            if not (isEditing ()) && not (isRunning ()) then
                selectedIndex ()
                |> Option.bind (fun index -> cells |> List.tryItem index |> Option.map (fun cell -> index, cell))
                |> Option.iter (fun (index, cell) ->
                    cellClipboard <- Some cell
                    cells <- cells |> removeCellAt index
                    selectIndex index

                    markDocumentDirty ()
                    setStatus "Cut cell."
                    rebuildCells ())

        let pasteCellBelow () =
            if not (isEditing ()) && not (isRunning ()) then
                cellClipboard
                |> Option.iter (fun cell ->
                    let insertIndex = selectedIndex () |> Option.map ((+) 1) |> Option.defaultValue 0

                    cells <- cells |> insertCellAt insertIndex (cloneCell cell)
                    selectIndex insertIndex
                    markDocumentDirty ()
                    setStatus "Pasted cell below."
                    rebuildCells ())

        let convertSelectedCell kind =
            if not (isEditing ()) && not (isRunning ()) then
                selectedIndex ()
                |> Option.bind (fun index -> cells |> List.tryItem index |> Option.map (fun cell -> index, cell))
                |> Option.filter (fun (_, cell) -> cell.Kind <> kind)
                |> Option.iter (fun (index, cell) ->
                    cells <- cells |> replaceCellAt index { cell with Kind = kind; Outputs = [] }
                    markDocumentDirty ()

                    setStatus (
                        match kind with
                        | CellKind.Markdown -> "Converted cell to Markdown."
                        | CellKind.Code -> "Converted cell to code."
                    )

                    rebuildCells ())

        let reloadFromDisk message =
            documentPath
            |> Option.iter (fun path ->
                let nextParsed, nextWriteTimeUtc = FilePersistence.load path
                parsed <- nextParsed
                lastWriteTimeUtc <- nextWriteTimeUtc
                cells <- parsed.Document.Cells
                highlightedCodeCache.Clear()

                selectIndex (selectedIndexOrZero ())

                markDocumentClean ()
                withMode Selecting
                setStatus message
                rebuildCells ())

        let saveAtPath path =
            try
                let saveStatus =
                    if hasExternalChanges () then
                        "Saved; external file changes overwritten."
                    else
                        "Saved."

                let nextParsed, nextWriteTimeUtc = FilePersistence.save path cells
                documentPath <- Some path
                parsed <- nextParsed
                lastWriteTimeUtc <- nextWriteTimeUtc
                cells <- parsed.Document.Cells
                highlightedCodeCache.Clear()

                selectIndex (selectedIndexOrZero ())

                markDocumentClean ()
                setStatus saveStatus
                rebuildCells ()
            with ex ->
                setStatus $"Save failed: {ex.Message}"

        let cancelSavePrompt () =
            showStatus ()
            setStatus "Save cancelled."

        let completeSavePrompt (input: TextBox) =
            let fileName = input.Text.Trim()

            if String.IsNullOrWhiteSpace fileName then
                input.Watermark <- "Enter a file name"
            else
                try
                    let path =
                        if Path.IsPathFullyQualified fileName then
                            Path.GetFullPath fileName
                        else
                            Path.GetFullPath(fileName, Environment.CurrentDirectory)

                    showStatus ()
                    saveAtPath path
                with ex ->
                    showStatus ()
                    setStatus $"Invalid file name: {ex.Message}"

        let beginSavePrompt () =
            let input =
                TextBox(Text = "", Watermark = "file.fsx", HorizontalAlignment = HorizontalAlignment.Stretch)

            let prompt = DockPanel(Background = theme.Dark)

            let label =
                TextBlock(Text = "Save as: ", Foreground = theme.Muted, Background = theme.Dark)

            DockPanel.SetDock(label, Dock.Left)
            prompt.Children.Add(label) |> ignore
            prompt.Children.Add(input) |> ignore
            savePrompt <- SavePromptActive input
            footer.Content <- prompt

            Dispatcher.UIThread.Post(fun () ->
                input.Focus() |> ignore
                input.CaretIndex <- input.Text.Length)

        let saveToDisk () =
            applySelectedEdit ()

            match documentPath with
            | Some path -> saveAtPath path
            | None -> beginSavePrompt ()

        let handleSavePrompt (args: KeyEventArgs) =
            match savePrompt with
            | SavePromptActive input when args.Key = Key.Enter ->
                args.Handled <- true
                completeSavePrompt input
                true
            | SavePromptActive _ when args.Key = Key.Escape ->
                args.Handled <- true
                cancelSavePrompt ()
                true
            | SavePromptActive _ -> true
            | SavePromptInactive -> false

        let checkExternalChange () =
            documentPath
            |> Option.iter (fun path ->
                let fileChanged = FilePersistence.hasChanged path lastWriteTimeUtc

                match
                    FilePersistence.decideExternalChange
                        persistenceMode
                        fileChanged
                        (isDirty ())
                        (isEditing ())
                        (hasExternalChanges ())
                with
                | IgnoreExternalChange -> ()
                | ReloadExternalChange -> reloadFromDisk "Reloaded external file changes."
                | KeepInMemoryAndNotify ->
                    markExternalChanged ()
                    setStatus "File changed on disk; in-memory edits kept.")

        let endEditing () =
            if isEditing () then
                applySelectedEdit ()
                withMode Selecting

                if hasExternalChanges () && not (isDirty ()) then
                    reloadFromDisk "Reloaded external file changes."
                else
                    rebuildCells ()

        let executeCellAsync selectedIndex source =
            task {
                try
                    let! result = fsi.ExecuteAsync(source, CancellationToken.None)
                    return selectedIndex, result.Output, None
                with ex ->
                    return selectedIndex, NotebookOutput.Error ex.Message, Some ex.Message
            }

        let runSelectedAsync () =
            task {
                match selectedRunnableCell () with
                | Some(selectedIndex, cell) when not (isRunning ()) ->
                    applySelectedEdit ()

                    let source =
                        cells
                        |> List.tryItem selectedIndex
                        |> Option.map _.Source
                        |> Option.defaultValue cell.Source

                    withMode Running
                    setStatus "Running selected cell..."
                    rebuildCells ()

                    let! selectedIndex, output, error = executeCellAsync selectedIndex source

                    do!
                        Dispatcher.UIThread.InvokeAsync(fun () ->
                            cells <- cells |> replaceCellOutput selectedIndex output
                            withMode Selecting

                            match error with
                            | Some _ -> setStatus "Execution failed."
                            | None -> setStatus "Ready"

                            rebuildCells ())
                | _ -> ()
            }

        let runAllAsync () =
            task {
                let runnableCells = runnableCells ()

                if not (isRunning ()) && not (List.isEmpty runnableCells) then
                    applySelectedEdit ()
                    withMode Running
                    setStatus (sprintf "Running %d code cells..." runnableCells.Length)
                    rebuildCells ()

                    let mutable failed = false

                    for index, cell in runnableCells do
                        if not failed then
                            do!
                                Dispatcher.UIThread.InvokeAsync(fun () ->
                                    selection <- Selected index
                                    setStatus (sprintf "Running cell %d/%d..." (index + 1) cells.Length)
                                    rebuildCells ())

                            let source =
                                cells
                                |> List.tryItem index
                                |> Option.map _.Source
                                |> Option.defaultValue cell.Source

                            let! selectedIndex, output, error = executeCellAsync index source

                            do!
                                Dispatcher.UIThread.InvokeAsync(fun () ->
                                    cells <- cells |> replaceCellOutput selectedIndex output
                                    rebuildCells ())

                            failed <- error.IsSome

                    do!
                        Dispatcher.UIThread.InvokeAsync(fun () ->
                            withMode Selecting

                            if failed then
                                setStatus "Run all stopped after a failed cell."
                            else
                                setStatus "Ready"

                            rebuildCells ())
            }

        let scroll =
            ScrollViewer(Content = cellStack, Background = theme.Dark, Focusable = false)

        scroll.ScrollChanged.Add(fun _ -> cellStack.InvalidateVisual())




        DockPanel.SetDock(header, Dock.Top)
        DockPanel.SetDock(footer, Dock.Bottom)
        header.Children.Add(headerText) |> ignore
        root.Children.Add(header) |> ignore
        root.Children.Add(footer) |> ignore
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
                if args.Handled then
                    ()
                elif handleSavePrompt args then
                    ()
                elif args.KeyModifiers = KeyModifiers.Control && args.Key = Key.C then
                    args.Handled <- true
                    requestQuit ()
                elif args.KeyModifiers = KeyModifiers.Control && args.Key = Key.S then
                    args.Handled <- true
                    saveToDisk ()
                elif args.Key = Key.F5 && (selectedRunnableCell () |> Option.isSome) then
                    args.Handled <- true
                    runSelectedAsync () |> ignore
                elif args.Key = Key.F6 && (runnableCells () |> List.isEmpty |> not) then
                    args.Handled <- true
                    runAllAsync () |> ignore
                elif args.Key = Key.A && args.KeyModifiers = KeyModifiers.None && not (isEditing ()) then
                    args.Handled <- true
                    addCellAbove ()
                elif args.Key = Key.B && args.KeyModifiers = KeyModifiers.None && not (isEditing ()) then
                    args.Handled <- true
                    addCellBelow ()
                elif args.Key = Key.X && args.KeyModifiers = KeyModifiers.None && not (isEditing ()) then
                    args.Handled <- true
                    cutCell ()
                elif args.Key = Key.C && args.KeyModifiers = KeyModifiers.None && not (isEditing ()) then
                    args.Handled <- true
                    copyCell ()
                elif args.Key = Key.V && args.KeyModifiers = KeyModifiers.None && not (isEditing ()) then
                    args.Handled <- true
                    pasteCellBelow ()
                elif args.Key = Key.M && args.KeyModifiers = KeyModifiers.None && not (isEditing ()) then
                    args.Handled <- true
                    convertSelectedCell CellKind.Markdown
                elif args.Key = Key.Y && args.KeyModifiers = KeyModifiers.None && not (isEditing ()) then
                    args.Handled <- true
                    convertSelectedCell CellKind.Code
                elif args.Key = Key.Down && not (isEditing ()) then
                    args.Handled <- true
                    moveSelection 1
                elif args.Key = Key.Up && not (isEditing ()) then
                    args.Handled <- true
                    moveSelection -1
                elif args.Key = Key.Enter && not (isEditing ()) then
                    args.Handled <- true
                    beginEditing ()
                elif args.Key = Key.Escape && isEditing () then
                    args.Handled <- true
                    endEditing ()
                elif args.Key = Key.Escape && uiState.QuitConfirmation <> Hidden then
                    args.Handled <- true
                    cancelQuitConfirmation ()),
            RoutingStrategies.Tunnel,
            true
        )

    override _.OnClosed(args) =
        completionService.Dispose()
        (fsi :> IDisposable).Dispose()
        base.OnClosed(args)

type App(path: string option, configuration: LfsiConfiguration) =
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
            AppBuilder.Configure(fun () -> App(path, configuration)).UseConsolonia()

        let builder =
            match TerminalGraphics.currentEnvironment () |> TerminalGraphics.decide with
            | UseTerminalGraphics Sixel -> builder.UseAutoDetectConsoleColorMode().UseAutoDetectedConsole()
            | _ -> builder.UseAutoDetectedConsole()

        builder.LogToException()

    [<EntryPoint>]
    let main argv =
        match argv |> Array.toList with
        | "--html" :: file :: _ when File.Exists file ->
            let html = LiterateScript.toHtml (Some file) (File.ReadAllText file)
            printf "%s" html
            0
        | file :: _ when File.Exists file -> (buildApp (Some(Path.GetFullPath file))).StartWithConsoleLifetime(argv)
        | [] -> (buildApp None).StartWithConsoleLifetime(argv)
        | _ ->
            eprintfn "File not found."
            1
