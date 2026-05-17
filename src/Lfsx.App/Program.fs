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

    // .NET environment mutation is not visible to native getenv on Unix.
    let setIfMissing name value =
        if not (OperatingSystem.IsWindows())
           && (Environment.GetEnvironmentVariable(name) |> String.IsNullOrWhiteSpace) then
            setenv(name, value, 1) |> ignore

type QuitConfirmation =
    | Hidden
    | Arming
    | Armed

type NotebookTheme =
    { Dark: SolidColorBrush
      Panel: SolidColorBrush
      Text: SolidColorBrush
      Muted: SolidColorBrush
      Accent: SolidColorBrush }

type NotebookViewModel(path: string option) =
    let source =
        path
        |> Option.filter File.Exists
        |> Option.map File.ReadAllText
        |> Option.defaultValue "printfn \"hello from lfsx\"\n1 + 1"

    let parsed = LiterateScript.parse path source

    member val Path = path with get
    member val Cells = parsed.Document.Cells with get, set
    member val SelectedIndex = 0 with get, set
    member val Diagnostics = parsed.FormattingDiagnostics with get

    member this.SelectedCell =
        this.Cells |> List.tryItem this.SelectedIndex

    member this.ReplaceSelected source =
        this.Cells <-
            this.Cells
            |> List.mapi (fun index cell ->
                if index = this.SelectedIndex then { cell with Source = source } else cell)

    member this.SetSelectedOutput output =
        this.Cells <-
            this.Cells
            |> List.mapi (fun index cell ->
                if index = this.SelectedIndex then { cell with Outputs = [ output ] } else cell)

    member this.MoveSelection delta =
        if not this.Cells.IsEmpty then
            let last = this.Cells.Length - 1
            this.SelectedIndex <- Math.Clamp(this.SelectedIndex + delta, 0, last)

type NotebookWindow(path: string option) as this =
    inherit Window(Title = "lfsx notebook", WindowState = WindowState.Maximized)

    let viewModel = NotebookViewModel(path)
    let fsi = new FsiSession(path |> Option.map Path.GetDirectoryName |> Option.defaultValue Environment.CurrentDirectory)

    let root = DockPanel()
    let header = TextBlock()
    let status = TextBlock()
    let scroll = ScrollViewer()
    let cellStack = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)
    let mutable selectedEditor: TextBox option = None
    let mutable running = false
    let mutable suppressEditorChange = false
    let quitConfirmationMessage = "Press Ctrl+C again to quit, or Esc to cancel."
    let mutable quitConfirmation = Hidden

    let theme =
        { Dark = SolidColorBrush(Color.FromRgb(18uy, 18uy, 18uy))
          Panel = SolidColorBrush(Color.FromRgb(28uy, 30uy, 34uy))
          Text = SolidColorBrush(Color.FromRgb(232uy, 232uy, 232uy))
          Muted = SolidColorBrush(Color.FromRgb(170uy, 176uy, 184uy))
          Accent = SolidColorBrush(Color.FromRgb(140uy, 190uy, 255uy)) }

    let selected = SolidColorBrush(Color.FromRgb(38uy, 72uy, 118uy))
    let error = SolidColorBrush(Color.FromRgb(255uy, 150uy, 150uy))

    let cellKindLabel kind =
        match kind with
        | CellKind.Markdown -> "markdown"
        | CellKind.Code -> "fsx"

    let outputText outputValue =
        match outputValue with
        | NotebookOutput.Text value -> value
        | NotebookOutput.Html value -> "[html]\n" + value
        | NotebookOutput.Error value -> "[error]\n" + value

    let setStatus message =
        status.Text <- message

    let formattingStatus () =
        if viewModel.Diagnostics.IsEmpty then
            "FSharp.Formatting parse: ok"
        else
            "FSharp.Formatting parse: " + String.concat "; " viewModel.Diagnostics

    let restoreStatus () =
        status.Text <- formattingStatus()

    let quit () =
        match Application.Current.ApplicationLifetime with
        | :? IControlledApplicationLifetime as lifetime -> lifetime.Shutdown()
        | _ -> this.Close()

    let requestQuit () =
        match quitConfirmation with
        | Hidden ->
            quitConfirmation <- Arming
            setStatus quitConfirmationMessage

            Task.Delay(1).ContinueWith(fun _ ->
                if quitConfirmation = Arming then
                    quitConfirmation <- Armed) |> ignore
        | Arming ->
            setStatus quitConfirmationMessage
        | Armed ->
            quit()

    let cancelQuitConfirmation () =
        if quitConfirmation <> Hidden then
            quitConfirmation <- Hidden
            restoreStatus()

    let saveSelectedText () =
        selectedEditor
        |> Option.iter (fun editor ->
            let cleaned =
                editor.Text
                    .Replace("[1;3A", "")
                    .Replace("[1;3B", "")
                    .Replace("\u001b[1;3A", "")
                    .Replace("\u001b[1;3B", "")

            viewModel.ReplaceSelected cleaned)

    let saveFile () =
        saveSelectedText()

        match path with
        | None -> setStatus "No file path. Start lfsx with a .fsx path to save."
        | Some target ->
            let doc = { SourcePath = path; Cells = viewModel.Cells }
            File.WriteAllText(target, NotebookDocument.source doc)
            setStatus("Saved " + target)

    let focusEditor () =
        selectedEditor
        |> Option.iter (fun editor ->
            Dispatcher.UIThread.Post(fun () ->
                editor.Focus() |> ignore
                editor.CaretIndex <- editor.Text.Length))

    let rec rebuildCells () =
        selectedEditor <- None
        cellStack.Children.Clear()

        viewModel.Cells
        |> List.iteri (fun index cell ->
            let isSelected = index = viewModel.SelectedIndex

            let container =
                Border(
                    Background = (if isSelected then selected else theme.Panel),
                    Padding = Thickness(1.0),
                    Margin = Thickness(0.0, 0.0, 0.0, 1.0))

            let body = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)

            let title =
                TextBlock(
                    Text = sprintf "[%02d] %s" (index + 1) (cellKindLabel cell.Kind),
                    Foreground = (if isSelected then theme.Accent else theme.Muted))

            body.Children.Add(title) |> ignore

            if isSelected then
                let editor =
                    TextBox(
                        Text = cell.Source,
                        AcceptsReturn = true,
                        TextWrapping = TextWrapping.NoWrap,
                        Foreground = theme.Text,
                        Background = theme.Dark,
                        MinHeight = 3.0)

                editor.TextChanged.Add(fun _ ->
                    if not suppressEditorChange then
                        if editor.Text.Contains("[1;3B", StringComparison.Ordinal)
                           || editor.Text.Contains("\u001b[1;3B", StringComparison.Ordinal) then
                            suppressEditorChange <- true
                            editor.Text <-
                                editor.Text
                                    .Replace("[1;3B", "")
                                    .Replace("\u001b[1;3B", "")
                            suppressEditorChange <- false
                            moveSelection 1
                        elif editor.Text.Contains("[1;3A", StringComparison.Ordinal)
                             || editor.Text.Contains("\u001b[1;3A", StringComparison.Ordinal) then
                            suppressEditorChange <- true
                            editor.Text <-
                                editor.Text
                                    .Replace("[1;3A", "")
                                    .Replace("\u001b[1;3A", "")
                            suppressEditorChange <- false
                            moveSelection -1)

                editor.KeyDown.Add(handleNotebookKey)

                selectedEditor <- Some editor
                body.Children.Add(editor) |> ignore
            else
                let preview =
                    TextBlock(
                        Text = cell.Source.TrimEnd(),
                        Foreground = theme.Text,
                        TextWrapping = TextWrapping.NoWrap)

                body.Children.Add(preview) |> ignore

            if not cell.Outputs.IsEmpty then
                let renderedOutput =
                    cell.Outputs
                    |> List.map outputText
                    |> String.concat "\n\n"

                let outputBrush =
                    if cell.Outputs |> List.exists (function NotebookOutput.Error _ -> true | _ -> false) then error else theme.Muted

                body.Children.Add(
                    TextBlock(
                        Text = renderedOutput,
                        Foreground = outputBrush,
                        TextWrapping = TextWrapping.Wrap)) |> ignore

            container.Child <- body
            cellStack.Children.Add(container) |> ignore)

        header.Text <-
            sprintf "lfsx  cell %d/%d  Ctrl+R run  Ctrl+S save  Ctrl+C quit  Alt+Up/Down or Ctrl+P/N move  Tab/Shift+Tab move"
                (viewModel.SelectedIndex + 1)
                viewModel.Cells.Length

        if String.IsNullOrWhiteSpace status.Text then
            restoreStatus()

        focusEditor()

    and moveSelection delta =
        saveSelectedText()
        viewModel.MoveSelection delta
        rebuildCells()

    and runSelectedAsync () =
        task {
            if not running then
                running <- true
                saveSelectedText()
                setStatus "Running selected cell..."

                match viewModel.SelectedCell with
                | Some cell when cell.Kind = CellKind.Code ->
                    let! result = fsi.ExecuteAsync(cell.Source, CancellationToken.None)
                    viewModel.SetSelectedOutput result.Output
                | Some cell when cell.Kind = CellKind.Markdown ->
                    viewModel.SetSelectedOutput(NotebookOutput.Text cell.Source)
                | Some _ -> ()
                | None -> ()

                do! Dispatcher.UIThread.InvokeAsync(fun () ->
                    running <- false
                    setStatus "Ready"
                    rebuildCells())
        }

    and handleNotebookKey (args: KeyEventArgs) =
        if args.KeyModifiers.HasFlag KeyModifiers.Control && args.Key = Key.R then
            args.Handled <- true
            runSelectedAsync() |> ignore
        elif args.KeyModifiers.HasFlag KeyModifiers.Control && args.Key = Key.S then
            args.Handled <- true
            saveFile()
        elif args.KeyModifiers.HasFlag KeyModifiers.Control && args.Key = Key.N then
            args.Handled <- true
            moveSelection 1
        elif args.KeyModifiers.HasFlag KeyModifiers.Control && args.Key = Key.P then
            args.Handled <- true
            moveSelection -1
        elif args.KeyModifiers.HasFlag KeyModifiers.Alt && args.Key = Key.Down then
            args.Handled <- true
            moveSelection 1
        elif args.KeyModifiers.HasFlag KeyModifiers.Alt && args.Key = Key.Up then
            args.Handled <- true
            moveSelection -1
        elif args.Key = Key.Tab then
            args.Handled <- true
            moveSelection(if args.KeyModifiers.HasFlag KeyModifiers.Shift then -1 else 1)

    do
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark
        this.Background <- theme.Dark

        header.Foreground <- theme.Accent
        header.Background <- theme.Dark
        header.TextWrapping <- TextWrapping.Wrap
        status.Foreground <- theme.Muted
        status.Background <- theme.Dark
        status.TextWrapping <- TextWrapping.Wrap

        scroll.Content <- cellStack
        scroll.Background <- theme.Dark
        scroll.Focusable <- false

        DockPanel.SetDock(header, Dock.Top)
        DockPanel.SetDock(status, Dock.Bottom)
        root.Children.Add(header) |> ignore
        root.Children.Add(status) |> ignore
        root.Children.Add(scroll) |> ignore
        root.Background <- theme.Dark

        this.Content <- root

        this.KeyDown.Add(handleNotebookKey)

        this.AddHandler(InputElement.KeyDownEvent, (fun _ args ->
            if args.KeyModifiers = KeyModifiers.Control && args.Key = Key.C then
                args.Handled <- true
                requestQuit()
            elif args.Key = Key.Escape && quitConfirmation <> Hidden then
                args.Handled <- true
                cancelQuitConfirmation()),
            RoutingStrategies.Tunnel,
            true)

        rebuildCells()

    override _.OnOpened(args) =
        base.OnOpened(args)
        focusEditor()

    override _.OnClosed(args) =
        (fsi :> IDisposable).Dispose()
        base.OnClosed(args)

type App(path: string option) =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(ModernTheme())

    override _.OnFrameworkInitializationCompleted() =
        match base.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            desktop.MainWindow <- NotebookWindow(path)
        | _ -> ()

        base.OnFrameworkInitializationCompleted()

module Program =
    let private configureTerminalEnvironment () =
        NativeEnvironment.setIfMissing NativeEnvironment.EscDelayName NativeEnvironment.DefaultEscDelay

    let private buildApp path =
        configureTerminalEnvironment()

        AppBuilder
            .Configure(fun () -> App(path))
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
        | file :: _ when File.Exists file ->
            (buildApp (Some file)).StartWithConsoleLifetime(argv)
        | _ ->
            eprintfn "Pass an .fsx file path."
            1
