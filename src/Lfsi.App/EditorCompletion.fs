namespace Lfsi.App

open System
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Threading

module EditorCompletion =
    let private text (editor: TextBox) =
        editor.Text |> Option.ofObj |> Option.defaultValue ""

    let private setCaret index (editor: TextBox) =
        editor.CaretIndex <- index
        editor.SelectionStart <- index
        editor.SelectionEnd <- index

    let private prefixStart caretIndex (source: string) =
        let mutable index = Math.Clamp(caretIndex, 0, source.Length)

        let isIdentifierCharacter value =
            Char.IsLetterOrDigit value || value = '_' || value = char 39

        while index > 0 && isIdentifierCharacter source.[index - 1] do
            index <- index - 1

        index

    let private matchingItems caretIndex (source: string) items =
        let start = prefixStart caretIndex source
        let prefix = source.Substring(start, caretIndex - start)

        if String.IsNullOrEmpty prefix then
            items
        else
            items
            |> List.filter (fun item -> item.Label.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))

    let private shouldRequest force caretIndex (source: string) =
        if force then
            true
        elif caretIndex <= 0 || caretIndex > source.Length then
            false
        else
            let previous = source.[caretIndex - 1]

            Char.IsLetterOrDigit previous
            || previous = '_'
            || previous = char 39
            || previous = '.'

    let private updatePlacement (popup: Popup) caretIndex (source: string) =
        let caretIndex = Math.Clamp(caretIndex, 0, source.Length)
        let mutable line = 0
        let mutable column = 0

        for value in source.AsSpan(0, caretIndex) do
            if value = '\n' then
                line <- line + 1
                column <- 0
            else
                column <- column + 1

        popup.PlacementRect <- Rect(float column, float line, 1.0, 1.0)

    let attach
        (theme: NotebookTheme)
        (editor: TextBox)
        (service: ICompletionService)
        (getContext: string -> int -> CompletionContext)
        =
        let suggestions =
            ListBox(
                Background = theme.Panel,
                Foreground = theme.Text,
                Focusable = false,
                MaxHeight = 8.0,
                MinWidth = 24.0
            )

        let popup =
            Popup(
                PlacementTarget = editor,
                Placement = PlacementMode.BottomEdgeAlignedLeft,
                IsLightDismissEnabled = false,
                Child =
                    Border(
                        Background = theme.Panel,
                        BorderBrush = theme.Accent,
                        BorderThickness = Thickness(1.0),
                        Padding = Thickness(1.0),
                        Child = suggestions
                    )
            )

        let mutable items: CompletionItem list = []
        let mutable cancellation: CancellationTokenSource option = None
        let mutable suppressNextRequest = false

        let close () =
            cancellation |> Option.iter _.Cancel()
            cancellation |> Option.iter _.Dispose()
            cancellation <- None
            items <- []
            suggestions.ItemsSource <- null
            popup.IsOpen <- false

        let accept () =
            items
            |> List.tryItem suggestions.SelectedIndex
            |> Option.iter (fun item ->
                let source = text editor
                let caretIndex = Math.Clamp(editor.CaretIndex, 0, source.Length)
                let start = prefixStart caretIndex source
                suppressNextRequest <- true

                editor.Text <- source.Substring(0, start) + item.InsertText + source.Substring(caretIndex)
                setCaret (start + item.InsertText.Length) editor)

            close ()

        let request force =
            cancellation |> Option.iter _.Cancel()
            cancellation |> Option.iter _.Dispose()

            let requestCancellation = new CancellationTokenSource()
            cancellation <- Some requestCancellation
            let source = text editor
            let caretIndex = editor.CaretIndex

            task {
                if not force then
                    do! Task.Delay(180, requestCancellation.Token)

                if shouldRequest force caretIndex source then
                    let! completions = service.CompleteAsync(getContext source caretIndex, requestCancellation.Token)

                    do!
                        Dispatcher.UIThread.InvokeAsync(fun () ->
                            if
                                not requestCancellation.IsCancellationRequested
                                && text editor = source
                                && editor.CaretIndex = caretIndex
                            then
                                items <- completions |> matchingItems caretIndex source |> List.truncate 12

                                suggestions.ItemsSource <-
                                    items
                                    |> List.map (fun item ->
                                        match item.Detail with
                                        | Some detail -> item.Label + "  " + detail
                                        | None -> item.Label)

                                suggestions.SelectedIndex <- 0
                                updatePlacement popup caretIndex source
                                popup.IsOpen <- not items.IsEmpty)
                else
                    do! Dispatcher.UIThread.InvokeAsync(close)
            }
            |> ignore

        editor.TextChanged.Add(fun _ ->
            close ()

            if suppressNextRequest then
                suppressNextRequest <- false
            else
                request false)

        editor.AddHandler(
            InputElement.KeyDownEvent,
            (fun _ args ->
                if popup.IsOpen && args.Key = Key.Down then
                    args.Handled <- true
                    suggestions.SelectedIndex <- Math.Min(suggestions.SelectedIndex + 1, items.Length - 1)
                elif popup.IsOpen && args.Key = Key.Up then
                    args.Handled <- true
                    suggestions.SelectedIndex <- Math.Max(suggestions.SelectedIndex - 1, 0)
                elif popup.IsOpen && (args.Key = Key.Enter || args.Key = Key.Tab) then
                    args.Handled <- true
                    accept ()
                elif popup.IsOpen && args.Key = Key.Escape then
                    args.Handled <- true
                    close ()
                elif args.KeyModifiers = KeyModifiers.Control && args.Key = Key.Space then
                    args.Handled <- true
                    request true),
            RoutingStrategies.Tunnel,
            true
        )

        popup
