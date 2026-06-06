namespace Lfsi.App

open System.IO
open Lfsi.Core

type PersistenceMode =
    | NoPersistence
    | AutoReloadWhenClean

type ExternalChangeDecision =
    | IgnoreExternalChange
    | ReloadExternalChange
    | KeepInMemoryAndNotify

module FilePersistence =
    let newDocument () =
        { Document =
            { SourcePath = None
              Cells = [ NotebookCell.create CellKind.Code "" ] }
          FormattingDiagnostics = [] },
        System.DateTime.MinValue

    let private renderCell cell =
        match cell.Kind with
        | Code -> cell.Source
        | Markdown ->
            LiterateSyntax.markdownOpenToken
            + LiterateSyntax.unixNewline
            + cell.Source
            + LiterateSyntax.unixNewline
            + LiterateSyntax.markdownCloseToken

    let private separator previous next =
        match previous.Kind, next.Kind with
        | Code, Code ->
            LiterateSyntax.cellSpacing
            + LiterateSyntax.codeCellSeparator
            + LiterateSyntax.cellSpacing
        | _ -> LiterateSyntax.cellSpacing

    let toSource cells =
        cells
        |> List.mapFold
            (fun previous cell ->
                let prefix =
                    previous
                    |> Option.map (fun previous -> separator previous cell)
                    |> Option.defaultValue ""

                prefix + renderCell cell, Some cell)
            None
        |> fst
        |> String.concat ""

    let load path =
        let parsed = LiterateScript.parseCellsOnly (Some path) (File.ReadAllText path)

        let parsed =
            if parsed.Document.Cells.IsEmpty then
                { parsed with
                    Document =
                        { parsed.Document with
                            Cells = [ NotebookCell.create CellKind.Code "" ] } }
            else
                parsed

        parsed, File.GetLastWriteTimeUtc path

    let save path cells =
        File.WriteAllText(path, toSource cells)
        load path

    let canSave mode =
        match mode with
        | NoPersistence -> false
        | AutoReloadWhenClean -> true

    let hasChanged path lastWriteTimeUtc =
        File.Exists path && File.GetLastWriteTimeUtc path <> lastWriteTimeUtc

    let decideExternalChange mode fileChanged isDirty isEditing hasExternalChanges =
        match mode, fileChanged, isDirty || isEditing, hasExternalChanges with
        | NoPersistence, _, _, _ -> IgnoreExternalChange
        | AutoReloadWhenClean, false, _, _ -> IgnoreExternalChange
        | AutoReloadWhenClean, true, false, _ -> ReloadExternalChange
        | AutoReloadWhenClean, true, true, false -> KeepInMemoryAndNotify
        | AutoReloadWhenClean, true, true, true -> IgnoreExternalChange
