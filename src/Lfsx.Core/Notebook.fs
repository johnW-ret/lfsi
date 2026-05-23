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
    let private display mimeType payload =
        Display
            { MimeType = mimeType
              Payload = payload }

    let text value =
        display MimeTypes.Text (TextPayload value)

    let html value =
        display MimeTypes.Html (TextPayload value)

    let png bytes =
        display MimeTypes.Png (BinaryPayload bytes)

    let svg value =
        display MimeTypes.Svg (TextPayload value)

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
