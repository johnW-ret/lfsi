namespace Lfsx.App

open System.IO
open Lfsx.Core

type PersistenceMode =
    | NoPersistence
    | AutoReloadWhenClean

type ExternalChangeDecision =
    | IgnoreExternalChange
    | ReloadExternalChange
    | KeepInMemoryAndNotify

module FilePersistence =
    let load path =
        LiterateScript.parse (Some path) (File.ReadAllText path), File.GetLastWriteTimeUtc path

    let hasChanged path lastWriteTimeUtc =
        File.Exists path && File.GetLastWriteTimeUtc path <> lastWriteTimeUtc

    let decideExternalChange mode fileChanged isDirty isEditing hasExternalChanges =
        match mode, fileChanged, isDirty || isEditing, hasExternalChanges with
        | NoPersistence, _, _, _ -> IgnoreExternalChange
        | AutoReloadWhenClean, false, _, _ -> IgnoreExternalChange
        | AutoReloadWhenClean, true, false, _ -> ReloadExternalChange
        | AutoReloadWhenClean, true, true, false -> KeepInMemoryAndNotify
        | AutoReloadWhenClean, true, true, true -> IgnoreExternalChange
