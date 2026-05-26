namespace Lfsi.App

open System
open Avalonia.Controls
open Avalonia.Controls.Documents
open Avalonia.Media
open FSharp.Compiler.Tokenization

module SyntaxHighlighting =
    type FSharpCompilerService = private FSharpCompilerService of FSharpSourceTokenizer

    type SpanKind =
        | Default
        | Keyword
        | String
        | Comment
        | Number
        | Operator
        | Preprocessor

    type HighlightSpan = { Text: string; Kind: SpanKind }

    type Palette =
        { Default: IBrush
          Keyword: IBrush
          String: IBrush
          Comment: IBrush
          Number: IBrush
          Operator: IBrush
          Preprocessor: IBrush }

    let fsharpCompilerService =
        FSharpCompilerService(FSharpSourceTokenizer([], None, Some "notebook.fsx", Some false))

    let private normalizeNewlines (source: string) =
        source.Replace("\r\n", "\n").Replace("\r", "\n")

    let private kindForToken (token: FSharpTokenInfo) =
        match token.ColorClass with
        | FSharpTokenColorKind.Keyword -> Keyword
        | FSharpTokenColorKind.String -> String
        | FSharpTokenColorKind.Comment -> Comment
        | FSharpTokenColorKind.Number -> Number
        | FSharpTokenColorKind.Operator -> Operator
        | FSharpTokenColorKind.PreprocessorKeyword -> Preprocessor
        | _ -> Default

    let private appendSpan text kind spans =
        if String.IsNullOrEmpty text then
            spans
        else
            { Text = text; Kind = kind } :: spans

    let private tokenizeLine (tokenizer: FSharpSourceTokenizer) state (line: string) =
        let lineTokenizer = tokenizer.CreateLineTokenizer line

        let rec loop state cursor spans =
            match lineTokenizer.ScanToken state with
            | Some token, nextState ->
                let left = Math.Clamp(token.LeftColumn, 0, line.Length)
                let right = Math.Clamp(token.RightColumn + 1, left, line.Length)

                let spans =
                    spans
                    |> appendSpan (line.Substring(cursor, left - cursor)) Default
                    |> appendSpan (line.Substring(left, right - left)) (kindForToken token)

                loop nextState right spans
            | None, nextState ->
                let spans = appendSpan (line.Substring(cursor)) Default spans
                List.rev spans, nextState

        loop state 0 []

    type Mode =
        | None
        | Some of FSharpCompilerService

    let defaultMode = Some fsharpCompilerService

    let highlight mode source =
        match mode with
        | None -> [ { Text = source; Kind = Default } ]
        | Some(FSharpCompilerService tokenizer) ->
            let lines = normalizeNewlines source |> fun value -> value.Split '\n'
            let spans = ResizeArray<HighlightSpan>()

            let _ =
                lines
                |> Array.mapi (fun index line -> index, line)
                |> Array.fold
                    (fun state (index, line) ->
                        let lineSpans, nextState = tokenizeLine tokenizer state line

                        if index > 0 then
                            spans.Add { Text = "\n"; Kind = Default }

                        lineSpans |> List.iter spans.Add
                        nextState)
                    FSharpTokenizerLexState.Initial

            List.ofSeq spans

    let private brushFor palette kind =
        match kind with
        | Default -> palette.Default
        | Keyword -> palette.Keyword
        | String -> palette.String
        | Comment -> palette.Comment
        | Number -> palette.Number
        | Operator -> palette.Operator
        | Preprocessor -> palette.Preprocessor

    let updateTextBlock palette spans (block: TextBlock) =
        block.Inlines.Clear()

        spans
        |> List.iter (fun span ->
            block.Inlines.Add(Run(Text = span.Text, Foreground = brushFor palette span.Kind))
            |> ignore)

    let textBlock palette spans =
        let block = TextBlock(TextWrapping = TextWrapping.NoWrap)
        updateTextBlock palette spans block
        block
