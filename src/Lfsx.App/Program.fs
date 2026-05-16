namespace Lfsx.App

open System
open System.IO
open Lfsx.Core

module Program =
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
        match argv |> Array.toList with
        | "--html" :: file :: _ when File.Exists file ->
            let html = LiterateScript.toHtml (Some file) (File.ReadAllText file)
            printf "%s" html
            0
        | file :: _ when File.Exists file ->
            let source = File.ReadAllText file

            let parsed = LiterateScript.parse (Some file) source

            printfn "cells: %d" parsed.Document.Cells.Length

            parsed.Document.Cells
            |> List.iteri (fun index cell ->
                printfn "%02d [%s] %s" (index + 1) (cellLabel cell) (firstLine cell.Source))

            0
        | _ ->
            eprintfn "Pass an .fsx file path."
            1
