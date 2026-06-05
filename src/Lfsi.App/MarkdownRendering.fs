namespace Lfsi.App

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Documents
open Avalonia.Layout
open Avalonia.Media
open Markdig
open Markdig.Syntax
open Markdig.Syntax.Inlines

module MarkdownRendering =
    type InlineStyle = { Bold: bool; Italic: bool }

    let private plainStyle = { Bold = false; Italic = false }

    let private pipeline =
        MarkdownPipelineBuilder().UsePipeTables().UseTaskLists().Build()

    let private run theme style text =
        Run(
            Text = text,
            Foreground = theme.Text,
            FontWeight = (if style.Bold then FontWeight.Bold else FontWeight.Normal),
            FontStyle = (if style.Italic then FontStyle.Italic else FontStyle.Normal)
        )

    let private codeRun theme text =
        Run(
            Text = text,
            Foreground = theme.Accent,
            FontFamily = FontFamily.Parse "monospace",
            FontWeight = FontWeight.Normal
        )

    let rec private addInlines theme style (inlines: InlineCollection) (inlineNode: Inline) =
        match inlineNode with
        | :? LiteralInline as literal -> inlines.Add(run theme style (literal.Content.ToString())) |> ignore
        | :? CodeInline as code -> inlines.Add(codeRun theme code.Content) |> ignore
        | :? LineBreakInline -> inlines.Add(LineBreak()) |> ignore
        | :? EmphasisInline as emphasis ->
            let nextStyle =
                if emphasis.DelimiterCount >= 2 then
                    { style with Bold = true }
                else
                    { style with Italic = true }

            addInlineChildren theme nextStyle inlines emphasis.FirstChild
        | :? LinkInline as link ->
            let linkStyle = { style with Bold = true }
            addInlineChildren theme linkStyle inlines link.FirstChild
        | :? ContainerInline as container -> addInlineChildren theme style inlines container.FirstChild
        | _ -> ()

    and private addInlineChildren theme style inlines child =
        let rec loop (node: Inline) =
            if not (isNull node) then
                addInlines theme style inlines node
                loop node.NextSibling

        loop child

    let private textBlock theme =
        TextBlock(Foreground = theme.Text, TextWrapping = TextWrapping.Wrap)

    let private renderLeafBlock theme (block: LeafBlock) =
        let text = textBlock theme

        match block.Inline with
        | null -> text.Text <- block.Lines.ToString()
        | inlineContainer -> addInlineChildren theme plainStyle text.Inlines inlineContainer.FirstChild

        text :> Control

    let private renderHeading theme (heading: HeadingBlock) =
        let text =
            TextBlock(Foreground = theme.Accent, FontWeight = FontWeight.Bold, TextWrapping = TextWrapping.Wrap)

        let prefix = String.replicate heading.Level "#" + " "

        text.Inlines.Add(Run(Text = prefix, Foreground = theme.Muted, FontWeight = FontWeight.Bold))
        |> ignore

        match heading.Inline with
        | null -> ()
        | inlineContainer -> addInlineChildren theme plainStyle text.Inlines inlineContainer.FirstChild

        text :> Control

    let private renderCodeBlock theme (block: CodeBlock) =
        let code =
            TextBlock(
                Text = block.Lines.ToString().TrimEnd(),
                Foreground = theme.Text,
                FontFamily = FontFamily.Parse "monospace",
                TextWrapping = TextWrapping.NoWrap
            )

        Border(BorderBrush = theme.Muted, BorderThickness = Thickness(1.0), Padding = Thickness(1.0), Child = code)
        :> Control

    let rec private renderBlock theme (block: Block) : Control list =
        match block with
        | :? HeadingBlock as heading -> [ renderHeading theme heading ]
        | :? ParagraphBlock as paragraph -> [ renderLeafBlock theme paragraph ]
        | :? FencedCodeBlock as code -> [ renderCodeBlock theme code ]
        | :? CodeBlock as code -> [ renderCodeBlock theme code ]
        | :? QuoteBlock as quote -> [ renderContainer theme (Thickness(2.0, 0.0, 0.0, 0.0)) quote :> Control ]
        | :? ListBlock as list -> [ renderList theme list :> Control ]
        | :? ThematicBreakBlock -> [ TextBlock(Text = "────────", Foreground = theme.Muted) :> Control ]
        | :? ContainerBlock as container -> [ renderContainer theme (Thickness 0.0) container :> Control ]
        | :? LeafBlock as leaf -> [ renderLeafBlock theme leaf ]
        | _ -> []

    and private renderContainer theme margin (container: ContainerBlock) =
        let stack =
            StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0, Margin = margin)

        container
        |> Seq.cast<Block>
        |> Seq.collect (renderBlock theme)
        |> Seq.iter (fun control -> stack.Children.Add control |> ignore)

        stack

    and private renderList theme (list: ListBlock) =
        let stack = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)

        list
        |> Seq.cast<Block>
        |> Seq.mapi (fun index block -> index, block)
        |> Seq.iter (fun (index, block) ->
            match block with
            | :? ListItemBlock as item ->
                let bullet =
                    if list.IsOrdered then
                        let start =
                            match Int32.TryParse list.OrderedStart with
                            | true, value -> value
                            | false, _ -> 1

                        string (start + index) + ". "
                    else
                        "• "

                let row = StackPanel(Orientation = Orientation.Horizontal, Spacing = 1.0)
                row.Children.Add(TextBlock(Text = bullet, Foreground = theme.Muted)) |> ignore
                row.Children.Add(renderContainer theme (Thickness 0.0) item) |> ignore
                stack.Children.Add row |> ignore
            | _ -> ())

        stack

    let render theme source =
        let document = Markdown.Parse(source, pipeline)
        let stack = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)

        document
        |> Seq.cast<Block>
        |> Seq.collect (renderBlock theme)
        |> Seq.iter (fun control -> stack.Children.Add control |> ignore)

        stack :> Control
