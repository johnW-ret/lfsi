# lfsi

A notebook terminal editor and runner for F#.


Edit and create [fsx files](https://learn.microsoft.com/en-us/dotnet/fsharp/tools/fsharp-interactive/#scripting-with-f) like a notebook using cells in a terminal.

You can:

- Write Markdown cells using [Literate Script](https://fsprojects.github.io/FSharp.Formatting/literate.html)-ish syntax
  - separate code cells with `(** *)`
- Bring over your code from Polyglot Notebooks with visual outputs (non-interactive) from packages like [Plotly.NET](https://plotly.net/)
  - uses Chrome CDP rendering to render images in the terminal using kitty and sixel
  - (working but still a work in progress)

lfsi

- preserves in-memory edits but reloads other changes automatically (helpful for AI editing tools)
- supports simple syntax highlighting with `FSharp.Compiler.Service`

Invoke with `--help` to see available commands.