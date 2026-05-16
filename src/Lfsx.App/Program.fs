namespace Lfsx.App

open System
open System.IO
open Lfsx.Core

module Program =
    let private cellKindLabel kind =
        match kind with
        | CellKind.Markdown -> "markdown"
        | CellKind.Code -> "fsx"

    let private firstLine (source: string) =
        source.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryHead
        |> Option.defaultValue "(empty)"

    let private formattingStatus diagnostics =
        if diagnostics |> List.isEmpty then
            "FSharp.Formatting parse: ok"
        else
            "FSharp.Formatting parse: " + String.concat "; " diagnostics

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

            eprintfn "cells: %d" parsed.Document.Cells.Length
            eprintfn "%s" (formattingStatus parsed.FormattingDiagnostics)

            parsed.Document.Cells
            |> List.iteri (fun index cell ->
                eprintfn "%02d [%s] %s" (index + 1) (cellKindLabel cell.Kind) (firstLine cell.Source))

            0
        | _ ->
            eprintfn "Pass an .fsx file path."
            1
