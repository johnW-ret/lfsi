namespace Lfsx.Core

open System

type CellKind =
    | Markdown
    | Code

type NotebookOutput =
    | Text of string
    | Html of string
    | Error of string

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
