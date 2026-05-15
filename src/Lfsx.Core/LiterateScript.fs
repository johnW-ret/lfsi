namespace Lfsx.Core

open System
open FSharp.Formatting.Literate

type LiterateParseResult =
    {
        Document: NotebookDocument
        FormattingDiagnostics: string list
    }

module LiterateScript =
    let private normalizeNewlines (text: string) =
        text.Replace("\r\n", "\n").Replace("\r", "\n")

    let private trimOneLeadingNewline (text: string) =
        if text.StartsWith("\n", StringComparison.Ordinal) then text.Substring(1) else text

    let private trimOneTrailingNewline (text: string) =
        if text.EndsWith("\n", StringComparison.Ordinal) then text.Substring(0, text.Length - 1) else text

    let private block kind source =
        NotebookCell.create kind (source |> trimOneLeadingNewline |> trimOneTrailingNewline)

    let private isSingleLineEmptyMarkdownSeparator (source: string) startIndex endIndex =
        let openTokenLength = 3
        let body = source.Substring(startIndex + openTokenLength, endIndex - startIndex - openTokenLength)
        not (body.Contains("\n", StringComparison.Ordinal)) && String.IsNullOrWhiteSpace body

    let private addCodeCell (buffer: Text.StringBuilder) =
        if buffer.Length = 0 then
            None
        else
            let source = buffer.ToString() |> trimOneTrailingNewline
            buffer.Clear() |> ignore

            if String.IsNullOrWhiteSpace source then
                None
            else
                Some(block CellKind.Code source)

    let private findToken (source: string) token start =
        let index = source.IndexOf(token, start, StringComparison.Ordinal)
        if index < 0 then None else Some index

    let private findMarkdownStart (source: string) start =
        let rec loop cursor =
            let index = source.IndexOf("(**", cursor, StringComparison.Ordinal)

            if index < 0 then
                None
            elif index + 3 < source.Length && source.[index + 3] = '*' then
                loop (index + 3)
            else
                Some index

        loop start

    let parse (sourcePath: string option) (source: string) =
        let source = normalizeNewlines source
        let cells = ResizeArray<NotebookCell>()
        let codeBuffer = Text.StringBuilder()
        let mutable cursor = 0

        while cursor < source.Length do
            match findMarkdownStart source cursor with
            | None ->
                codeBuffer.Append(source.Substring(cursor)) |> ignore
                cursor <- source.Length
            | Some startIndex ->
                if startIndex > cursor then
                    codeBuffer.Append(source.Substring(cursor, startIndex - cursor)) |> ignore

                addCodeCell codeBuffer
                |> Option.iter cells.Add

                match findToken source "*)" (startIndex + 3) with
                | None ->
                    codeBuffer.Append(source.Substring(startIndex)) |> ignore
                    cursor <- source.Length
                | Some endIndex ->
                    let bodyStart = startIndex + 3
                    let body = source.Substring(bodyStart, endIndex - bodyStart)

                    if not (isSingleLineEmptyMarkdownSeparator source startIndex endIndex) then
                        cells.Add(block CellKind.Markdown body)

                    cursor <- endIndex + 2

        addCodeCell codeBuffer
        |> Option.iter cells.Add

        {
            Document = { SourcePath = sourcePath; Cells = cells |> Seq.toList }
            FormattingDiagnostics =
                try
                    let parsed: LiterateDocument =
                        Literate.ParseScriptString(source, ?path = sourcePath)

                    parsed.Diagnostics
                    |> Seq.map string
                    |> Seq.toList
                with ex ->
                    [ ex.Message ]
        }

    let toHtml (sourcePath: string option) (source: string) =
        let parsed = Literate.ParseScriptString(source, ?path = sourcePath)

        Literate.ToHtml(parsed, prefix = "", lineNumbers = false, generateAnchors = false)
