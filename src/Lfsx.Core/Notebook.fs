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
    {
        Id: Guid
        Kind: CellKind
        Source: string
        Outputs: NotebookOutput list
    }

module NotebookCell =
    let create kind source =
        {
            Id = Guid.NewGuid()
            Kind = kind
            Source = source
            Outputs = []
        }

    let withOutput output cell =
        { cell with Outputs = cell.Outputs @ [ output ] }

type NotebookDocument =
    {
        SourcePath: string option
        Cells: NotebookCell list
    }

module NotebookDocument =
    let empty = { SourcePath = None; Cells = [] }

    let private cellSource cell =
        match cell.Kind with
        | CellKind.Markdown ->
            "(**\n" + cell.Source.TrimEnd() + "\n*)"
        | CellKind.Code ->
            cell.Source.TrimEnd()

    let source doc =
        doc.Cells
        |> List.fold
            (fun (previousKind, parts) cell ->
                let separator =
                    match previousKind, cell.Kind with
                    | Some CellKind.Code, CellKind.Code -> "\n\n(** *)\n\n"
                    | _ -> "\n\n"

                let next =
                    match parts with
                    | [] -> [ cellSource cell ]
                    | _ -> cellSource cell :: separator :: parts

                Some cell.Kind, next)
            (None, [])
        |> snd
        |> List.rev
        |> String.concat ""
