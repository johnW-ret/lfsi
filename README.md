# lfsx

`lfsx` is an F# terminal notebook editor for ordinary `.fsx` scripts and FSharp.Formatting literate scripts.

The project intentionally avoids a JSON notebook format. The saved artifact is still an `.fsx` file, with optional FSharp.Formatting literate comments:

- Markdown cells: `(** ... *)`
- Adjacent code-cell separator: single-line empty literate comments such as `(** *)`
- Everything else: normal F# script code

FSharp.Formatting output commands such as `(*** include-value: name ***)` are not notebook primitives in `lfsx`; they remain ordinary `.fsx` comment text inside code cells. Notebook output is produced by running code cells with `fsi`.

## Projects

- `src/Lfsx.Core`: notebook document model, literate parser, FSharp.Formatting HTML export, output classification, and an `fsi` session wrapper.
- `src/Lfsx.App`: a small Consolonia terminal UI over `Lfsx.Core`.
- `tests/Lfsx.Tests`: xUnit tests for parser and document behavior.

## Run

```powershell
$env:DOTNET_CLI_HOME="$PWD/.dotnet"
dotnet run --project src/Lfsx.App -- examples/hello.fsx
```

Export the FSharp.Formatting HTML for a script:

```powershell
$env:DOTNET_CLI_HOME="$PWD/.dotnet"
dotnet run --project src/Lfsx.App -- --html examples/hello.fsx
```

## Current Slice

The editor shows cells, lets you edit the selected cell, run code cells through `dotnet fsi`, and save back to the source `.fsx`.

HTML output classification is already represented in `Lfsx.Core.OutputRendering`. The next step is to plug in a real HTML-to-image renderer behind `IHtmlImageRenderer` and surface that through a Consolonia control; the current UI displays HTML text as a placeholder.

## Keyboard

- `Tab` / `Shift+Tab`: move between cells
- `Alt+Down` / `Alt+Up`: move between cells
- `Ctrl+N` / `Ctrl+P`: move between cells in terminals that do not report Alt+Arrow cleanly
- `Ctrl+R`: run the selected cell
- `Ctrl+S`: save the notebook back to the `.fsx`
- `Ctrl+Q`: quit
