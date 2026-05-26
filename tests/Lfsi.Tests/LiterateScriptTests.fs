namespace Lfsi.Tests

open System
open System.IO
open Expecto
open Lfsi.Core

module LiterateScriptTests =
    [<Tests>]
    let tests =
        testList
            "LiterateScript"
            [ testCase "parse preserves empty code cells around code separators"
              <| fun _ ->
                  let source =
                      LiterateSyntax.cellSpacing
                      + LiterateSyntax.codeCellSeparator
                      + LiterateSyntax.cellSpacing
                      + "let first = 1"
                      + LiterateSyntax.cellSpacing
                      + LiterateSyntax.codeCellSeparator
                      + LiterateSyntax.cellSpacing
                      + LiterateSyntax.cellSpacing
                      + LiterateSyntax.codeCellSeparator
                      + LiterateSyntax.cellSpacing
                      + "let second = 2"
                      + LiterateSyntax.cellSpacing
                      + LiterateSyntax.codeCellSeparator
                      + LiterateSyntax.cellSpacing

                  let cells = (LiterateScript.parse None source).Document.Cells

                  Expect.equal
                      (cells |> List.map _.Source)
                      [ ""; "let first = 1"; ""; "let second = 2"; "" ]
                      "cell sources"

                  Expect.equal (cells |> List.map _.Kind) [ Code; Code; Code; Code; Code ] "cell kinds"

              testCase "parse does not attach markdown spacing to following code cells"
              <| fun _ ->
                  let source =
                      LiterateSyntax.markdownOpenToken
                      + LiterateSyntax.unixNewline
                      + "markdown"
                      + LiterateSyntax.unixNewline
                      + LiterateSyntax.markdownCloseToken
                      + LiterateSyntax.cellSpacing
                      + "let value = 1"

                  let cells = (LiterateScript.parse None source).Document.Cells

                  Expect.equal (cells |> List.map _.Source) [ "markdown"; "let value = 1" ] "cell sources"
                  Expect.equal (cells |> List.map _.Kind) [ Markdown; Code ] "cell kinds"

              testCase "parse preserves leading blank lines in code cells after markdown"
              <| fun _ ->
                  let source =
                      LiterateSyntax.markdownOpenToken
                      + LiterateSyntax.unixNewline
                      + "markdown"
                      + LiterateSyntax.unixNewline
                      + LiterateSyntax.markdownCloseToken
                      + LiterateSyntax.cellSpacing
                      + LiterateSyntax.cellSpacing
                      + "let value = 1"

                  let cells = (LiterateScript.parse None source).Document.Cells

                  Expect.equal (cells |> List.map _.Source) [ "markdown"; "\n\nlet value = 1" ] "cell sources"
                  Expect.equal (cells |> List.map _.Kind) [ Markdown; Code ] "cell kinds"

              testCase "parse does not write formatting diagnostics to console"
              <| fun _ ->
                  let source = "This is markdown that was converted to code."
                  use output = new StringWriter()
                  use error = new StringWriter()
                  let originalOut = Console.Out
                  let originalError = Console.Error

                  try
                      Console.SetOut output
                      Console.SetError error
                      LiterateScript.parse (Some "broken.fsx") source |> ignore
                  finally
                      Console.SetOut originalOut
                      Console.SetError originalError

                  Expect.equal (output.ToString()) "" "stdout"
                  Expect.equal (error.ToString()) "" "stderr" ]
