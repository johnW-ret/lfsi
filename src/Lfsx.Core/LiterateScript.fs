namespace Lfsx.Core

open System
open FSharp.Formatting.Literate

type LiterateParseResult =
    { Document: NotebookDocument
      FormattingDiagnostics: string list }

module LiterateScript =
    let private commandCommentMarker = '*'
    let private notFoundIndex = -1

    let private normalizeNewlines (text: string) =
        text
            .Replace(LiterateSyntax.windowsNewline, LiterateSyntax.unixNewline)
            .Replace(LiterateSyntax.carriageReturn, LiterateSyntax.unixNewline)

    let private trimOneLeadingNewline (text: string) =
        if text.StartsWith(LiterateSyntax.unixNewline, StringComparison.Ordinal) then
            text.Substring(LiterateSyntax.unixNewline.Length)
        else
            text

    let private trimOneTrailingNewline (text: string) =
        if text.EndsWith(LiterateSyntax.unixNewline, StringComparison.Ordinal) then
            text.Substring(0, text.Length - LiterateSyntax.unixNewline.Length)
        else
            text

    let private block kind source =
        NotebookCell.create kind (source |> trimOneLeadingNewline |> trimOneTrailingNewline)

    let private isSingleLineEmptyMarkdownSeparator (source: string) startIndex endIndex =
        let body =
            source.Substring(
                startIndex + LiterateSyntax.markdownOpenToken.Length,
                endIndex - startIndex - LiterateSyntax.markdownOpenToken.Length
            )

        not (body.Contains(LiterateSyntax.unixNewline, StringComparison.Ordinal))
        && String.IsNullOrWhiteSpace body

    let private codeCell source =
        let source = source |> trimOneTrailingNewline

        if String.IsNullOrWhiteSpace source then
            None
        else
            Some(block CellKind.Code source)

    let private prependIfSome item items =
        item |> Option.map (fun x -> x :: items) |> Option.defaultValue items

    let private substring (source: string) startIndex endIndex =
        source.Substring(startIndex, endIndex - startIndex)

    let private findToken (source: string) token start =
        let index = source.IndexOf(token, start, StringComparison.Ordinal)
        if index = notFoundIndex then None else Some index

    let private findMarkdownStart (source: string) start =
        let rec loop cursor =
            let index =
                source.IndexOf(LiterateSyntax.markdownOpenToken, cursor, StringComparison.Ordinal)

            if index = notFoundIndex then
                None
            elif
                index + LiterateSyntax.markdownOpenToken.Length < source.Length
                && source.[index + LiterateSyntax.markdownOpenToken.Length] = commandCommentMarker
            then
                loop (index + LiterateSyntax.markdownOpenToken.Length)
            else
                Some index

        loop start

    let private parseCells (source: string) =
        let rec loop cursor cells =
            match findMarkdownStart source cursor with
            | None ->
                substring source cursor source.Length
                |> codeCell
                |> fun cell -> prependIfSome cell cells
                |> List.rev
            | Some startIndex ->
                let cells =
                    substring source cursor startIndex
                    |> codeCell
                    |> fun cell -> prependIfSome cell cells

                match
                    findToken
                        source
                        LiterateSyntax.markdownCloseToken
                        (startIndex + LiterateSyntax.markdownOpenToken.Length)
                with
                | None ->
                    substring source startIndex source.Length
                    |> codeCell
                    |> fun cell -> prependIfSome cell cells
                    |> List.rev
                | Some endIndex ->
                    let body =
                        substring source (startIndex + LiterateSyntax.markdownOpenToken.Length) endIndex

                    let cells =
                        if isSingleLineEmptyMarkdownSeparator source startIndex endIndex then
                            cells
                        else
                            block CellKind.Markdown body :: cells

                    loop (endIndex + LiterateSyntax.markdownCloseToken.Length) cells

        loop 0 []

    let parse (sourcePath: string option) (source: string) =
        let source = normalizeNewlines source

        { Document =
            { SourcePath = sourcePath
              Cells = parseCells source }
          FormattingDiagnostics =
            try
                let parsed: LiterateDocument =
                    Literate.ParseScriptString(source, ?path = sourcePath)

                parsed.Diagnostics |> Seq.map string |> Seq.toList
            with ex ->
                [ ex.Message ] }

    let toHtml (sourcePath: string option) (source: string) =
        let parsed = Literate.ParseScriptString(source, ?path = sourcePath)

        Literate.ToHtml(parsed, prefix = "", lineNumbers = false, generateAnchors = false)
