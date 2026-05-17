namespace Lfsx.App

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
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

type NotebookWindow(path: string) as this =
    inherit Window(Title = "lfsx notebook", WindowState = WindowState.Maximized)

    let parsed = LiterateScript.parse (Some path) (File.ReadAllText path)
    let quitConfirmationMessage = "Press Ctrl+C again to quit, or Esc to cancel."
    let mutable quitConfirmation = Hidden

    let formattingStatus () =
        if parsed.FormattingDiagnostics.IsEmpty then
            "FSharp.Formatting parse: ok"
        else
            "FSharp.Formatting parse: " + String.concat "; " parsed.FormattingDiagnostics

    let restoreStatus (status: TextBlock) =
        status.Text <- formattingStatus()

    let cellKindLabel kind =
        match kind with
        | CellKind.Markdown -> "markdown"
        | CellKind.Code -> "fsx"

    let addCellPreview theme (cellStack: StackPanel) index cell =
        let body = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)

        body.Children.Add(
            TextBlock(
                Text = sprintf "[%02d] %s" (index + 1) (cellKindLabel cell.Kind),
                Foreground = theme.Accent)) |> ignore

        body.Children.Add(
            TextBlock(
                Text = cell.Source.TrimEnd(),
                Foreground = theme.Text,
                TextWrapping = TextWrapping.NoWrap)) |> ignore

        cellStack.Children.Add(
            Border(
                Background = theme.Panel,
                Padding = Thickness(1.0),
                Margin = Thickness(0.0, 0.0, 0.0, 1.0),
                Child = body)) |> ignore

    do
        let theme =
            { Dark = SolidColorBrush(Color.FromRgb(18uy, 18uy, 18uy))
              Panel = SolidColorBrush(Color.FromRgb(28uy, 30uy, 34uy))
              Text = SolidColorBrush(Color.FromRgb(232uy, 232uy, 232uy))
              Muted = SolidColorBrush(Color.FromRgb(170uy, 176uy, 184uy))
              Accent = SolidColorBrush(Color.FromRgb(140uy, 190uy, 255uy)) }

        let root = DockPanel(Background = theme.Dark)
        let header =
            TextBlock(
                Text = sprintf "lfsx  %d cells  Ctrl+C quit" parsed.Document.Cells.Length,
                Foreground = theme.Accent,
                Background = theme.Dark,
                TextWrapping = TextWrapping.Wrap)

        let status =
            TextBlock(Text = formattingStatus(), Foreground = theme.Muted, Background = theme.Dark, TextWrapping = TextWrapping.Wrap)

        let closeWindow () =
            match Application.Current.ApplicationLifetime with
            | :? IControlledApplicationLifetime as lifetime -> lifetime.Shutdown()
            | _ -> this.Close()

        let cellStack = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)

        parsed.Document.Cells
        |> List.iteri (addCellPreview theme cellStack)

        let scroll = ScrollViewer(Content = cellStack, Background = theme.Dark)

        DockPanel.SetDock(header, Dock.Top)
        DockPanel.SetDock(status, Dock.Bottom)
        root.Children.Add(header) |> ignore
        root.Children.Add(status) |> ignore
        root.Children.Add(scroll) |> ignore

        base.RequestedThemeVariant <- Styling.ThemeVariant.Dark
        base.Background <- theme.Dark
        base.Content <- root

        this.AddHandler(InputElement.KeyDownEvent, (fun _ args ->
            if args.KeyModifiers = KeyModifiers.Control && args.Key = Key.C then
                args.Handled <- true

                match quitConfirmation with
                | Hidden ->
                    quitConfirmation <- Arming
                    status.Text <- quitConfirmationMessage

                    Task.Delay(1).ContinueWith(fun _ ->
                        if quitConfirmation = Arming then
                            quitConfirmation <- Armed) |> ignore
                | Arming ->
                    status.Text <- quitConfirmationMessage
                | Armed ->
                    closeWindow()
            elif args.Key = Key.Escape && quitConfirmation <> Hidden then
                args.Handled <- true
                quitConfirmation <- Hidden

                restoreStatus status),
            RoutingStrategies.Tunnel,
            true)

type App(path: string) =
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
            (buildApp file).StartWithConsoleLifetime(argv)
        | _ ->
            eprintfn "Pass an .fsx file path."
            1
