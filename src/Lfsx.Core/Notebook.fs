namespace Lfsx.Core

open System

type CellKind =
    | Markdown
    | Code

module MimeTypes =
    [<Literal>]
    let Text = "text/plain"

    [<Literal>]
    let Html = "text/html"

    [<Literal>]
    let Png = "image/png"

    [<Literal>]
    let Svg = "image/svg+xml"

    [<Literal>]
    let PlotlyJson = "application/vnd.plotly.v1+json"

type MimePayload =
    | TextPayload of string
    | BinaryPayload of byte[]

type MimeOutput =
    { MimeType: string
      Payload: MimePayload }

type NotebookOutput =
    | Display of MimeOutput
    | Error of string

module NotebookOutput =
    let text value =
        Display
            { MimeType = MimeTypes.Text
              Payload = TextPayload value }

    let html value =
        Display
            { MimeType = MimeTypes.Html
              Payload = TextPayload value }

    let png bytes =
        Display
            { MimeType = MimeTypes.Png
              Payload = BinaryPayload bytes }

    let svg value =
        Display
            { MimeType = MimeTypes.Svg
              Payload = TextPayload value }

    let plotlyJson value =
        Display
            { MimeType = MimeTypes.PlotlyJson
              Payload = TextPayload value }

    let isError output =
        match output with
        | Error _ -> true
        | Display _ -> false

type NotebookCell =
    { Id: Guid
      Kind: CellKind
      Source: string
      Outputs: NotebookOutput list }

module NotebookCell =
    let create kind source =
        { Id = Guid.NewGuid()
          Kind = kind
          Source = source
          Outputs = [] }


type NotebookDocument =
    { SourcePath: string option
      Cells: NotebookCell list }
