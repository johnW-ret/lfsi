namespace Lfsx.App

open System
open System.IO
open Lfsx.Core

module Program =
    let private defaultSource =
        "(**\n# Hello lfsx\n*)\n\nlet greeting name = $\"hello, {name}\"\n\ngreeting \"notebook\"\n\n(** *)\n\ngreeting \"again\""

    let private cellLabel cell =
        match cell.Kind with
        | CellKind.Markdown -> "markdown"
        | CellKind.Code -> "code"

    let private firstLine (source: string) =
        source.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryHead
        |> Option.defaultValue "(empty)"

    [<EntryPoint>]
    let main argv =
        let path = argv |> Array.tryHead

        let source =
            path
            |> Option.filter File.Exists
            |> Option.map File.ReadAllText
            |> Option.defaultValue defaultSource

        let parsed = LiterateScript.parse path source

        printfn "cells: %d" parsed.Document.Cells.Length

        parsed.Document.Cells
        |> List.iteri (fun index cell -> printfn "%02d [%s] %s" (index + 1) (cellLabel cell) (firstLine cell.Source))

        0
