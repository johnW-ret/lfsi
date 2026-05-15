namespace Lfsx.Core

open System
open System.IO
open System.Reflection
open System.Text.Encodings.Web

module OutputRendering =
    let private htmlContentInterfaceName = "Microsoft.AspNetCore.Html.IHtmlContent"

    let private tryRenderHtmlContent (value: obj) =
        if isNull value then
            None
        else
            let valueType = value.GetType()
            let htmlInterface =
                valueType.GetInterfaces()
                |> Array.tryFind (fun candidate -> candidate.FullName = htmlContentInterfaceName)

            match htmlInterface with
            | None -> None
            | Some htmlInterface ->
                let writeTo =
                    htmlInterface.GetMethod("WriteTo", [| typeof<TextWriter>; typeof<HtmlEncoder> |])

                if isNull writeTo then
                    None
                else
                    use writer = new StringWriter()
                    writeTo.Invoke(value, [| writer :> obj; HtmlEncoder.Default :> obj |]) |> ignore
                    Some(writer.ToString())

    let classify (value: obj) =
        match tryRenderHtmlContent value with
        | Some html -> NotebookOutput.Html html
        | None ->
            if isNull value then
                NotebookOutput.Text "<null>"
            else
                NotebookOutput.Text(sprintf "%A" value)

    type IHtmlImageRenderer =
        abstract RenderHtml: html: string -> byte[]
