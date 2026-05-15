module Lfsx.Tests

open Xunit
open Lfsx.Core

[<Fact>]
let ``plain fsx parses as one code cell`` () =
    let result = LiterateScript.parse None "let x = 1\nx + 1"

    Assert.Single(result.Document.Cells) |> ignore
    Assert.Equal(CellKind.Code, result.Document.Cells.Head.Kind)

[<Fact>]
let ``literate markdown and code parse into ordered cells`` () =
    let source = "(**\n# Title\n*)\n\nlet answer = 42\n\nanswer + 1"
    let result = LiterateScript.parse None source

    Assert.Equal(2, result.Document.Cells.Length)
    Assert.Equal(CellKind.Markdown, result.Document.Cells[0].Kind)
    Assert.Equal(CellKind.Code, result.Document.Cells[1].Kind)

[<Fact>]
let ``single line empty literate comment splits adjacent code cells`` () =
    let source = "let a = 1\na + 1\n\n(** *)\n\nlet b = 2\nb + 2"
    let result = LiterateScript.parse None source

    Assert.Equal(2, result.Document.Cells.Length)
    Assert.All(result.Document.Cells, fun cell -> Assert.Equal(CellKind.Code, cell.Kind))
    Assert.Contains("let a = 1", result.Document.Cells[0].Source)
    Assert.Contains("let b = 2", result.Document.Cells[1].Source)

[<Fact>]
let ``multiline empty literate comment remains markdown cell`` () =
    let source = "let a = 1\n\n(**\n*)\n\nlet b = 2"
    let result = LiterateScript.parse None source

    Assert.Equal(3, result.Document.Cells.Length)
    Assert.Equal(CellKind.Code, result.Document.Cells[0].Kind)
    Assert.Equal(CellKind.Markdown, result.Document.Cells[1].Kind)
    Assert.Equal("", result.Document.Cells[1].Source)
    Assert.Equal(CellKind.Code, result.Document.Cells[2].Kind)

[<Fact>]
let ``fsharp formatting commands remain part of code`` () =
    let source = "let answer = 42\n\n(*** include-value: answer ***)"
    let result = LiterateScript.parse None source

    Assert.Single(result.Document.Cells) |> ignore
    Assert.Equal(CellKind.Code, result.Document.Cells.Head.Kind)
    Assert.Contains("include-value", result.Document.Cells.Head.Source)

[<Fact>]
let ``document can roundtrip to literate fsx`` () =
    let doc =
        {
            SourcePath = None
            Cells =
                [
                    NotebookCell.create CellKind.Markdown "# Heading"
                    NotebookCell.create CellKind.Code "let x = 1"
                    NotebookCell.create CellKind.Code "x + 1"
                ]
        }

    let source = NotebookDocument.source doc

    Assert.Contains("(**", source)
    Assert.Contains("let x = 1", source)
    Assert.Contains("x + 1", source)

[<Fact>]
let ``document writes separator between adjacent code cells`` () =
    let doc =
        {
            SourcePath = None
            Cells =
                [
                    NotebookCell.create CellKind.Code "let a = 1"
                    NotebookCell.create CellKind.Code "let b = 2"
                ]
        }

    let source = NotebookDocument.source doc
    let reparsed = LiterateScript.parse None source

    Assert.Contains("(** *)", source)
    Assert.Equal(2, reparsed.Document.Cells.Length)
    Assert.All(reparsed.Document.Cells, fun cell -> Assert.Equal(CellKind.Code, cell.Kind))

[<Fact>]
let ``fsi session returns expression output`` () =
    task {
        use session = new FsiSession()
        let! result = session.ExecuteAsync("1 + 1", System.Threading.CancellationToken.None)

        match result.Output with
        | NotebookOutput.Text text -> Assert.Contains("2", text)
        | other -> failwithf "Expected text output, got %A" other
    }
