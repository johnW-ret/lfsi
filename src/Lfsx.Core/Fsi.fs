namespace Lfsx.Core

open System
open System.Diagnostics
open System.Text
open System.Threading
open System.Threading.Tasks

type FsiExecution =
    { Input: string
      Output: NotebookOutput }

type IFsiSession =
    inherit IDisposable
    abstract ExecuteAsync: code: string * cancellationToken: CancellationToken -> Task<FsiExecution>

type FsiSession(?workingDirectory: string) =
    [<Literal>]
    let MimeBeginMarker = "__LFSX_MIME_BEGIN__"

    [<Literal>]
    let MimeEndMarker = "__LFSX_MIME_END__"

    let workingDirectory = defaultArg workingDirectory Environment.CurrentDirectory

    let displayHelpers =
        """
module Lfsx =
    open System
    open System.IO
    open System.Reflection
    open System.Text

    let private emitEncodedMime (mime: string) (encoded: string) =
        printfn "__LFSX_MIME_BEGIN__%s" mime
        printfn "%s" encoded
        printfn "__LFSX_MIME_END__"

    let display (mime: string) (value: string) =
        value
        |> Encoding.UTF8.GetBytes
        |> Convert.ToBase64String
        |> emitEncodedMime mime

    let text (value: string) = display "text/plain" value
    let html (value: string) = display "text/html" value
    let svg (value: string) = display "image/svg+xml" value
    let plotlyJson (value: string) = display "application/vnd.plotly.v1+json" value
    let pngBase64 (value: string) = emitEncodedMime "image/png" value

    let private hasInterface interfaceName (valueType: Type) =
        valueType.GetInterfaces()
        |> Array.tryFind (fun interfaceType -> interfaceType.FullName = interfaceName)

    let private tryInvokeHtmlMethod (value: obj) =
        let valueType = value.GetType()
        let chartType =
            if not (isNull valueType.BaseType) && valueType.BaseType.FullName = "Plotly.NET.GenericChart" then
                valueType.BaseType
            else
                valueType

        let htmlMethod name =
            chartType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.tryFind (fun method ->
                method.Name = name
                && method.ReturnType = typeof<string>
                && method.GetParameters().Length = 1
                && method.GetParameters().[0].ParameterType.IsAssignableFrom(valueType))

        htmlMethod "toEmbeddedHTML"
        |> Option.orElseWith (fun () -> htmlMethod "toChartHTML")
        |> Option.map (fun method -> method.Invoke(null, [| value |]) :?> string)

    let private tryRenderHtmlContent (value: obj) =
        let valueType = value.GetType()

        match hasInterface "Microsoft.AspNetCore.Html.IHtmlContent" valueType with
        | Some htmlContent ->
            let writeTo = htmlContent.GetMethod("WriteTo")
            let encoderType = Type.GetType("System.Text.Encodings.Web.HtmlEncoder, System.Text.Encodings.Web")

            if isNull writeTo || isNull encoderType then
                None
            else
                let defaultEncoder = encoderType.GetProperty("Default").GetValue(null)
                use writer = new StringWriter()
                writeTo.Invoke(value, [| writer :> obj; defaultEncoder |]) |> ignore
                Some(writer.ToString())
        | None -> None

    let tryDisplayValue (value: obj) =
        if isNull value then
            false
        else
            let valueType = value.GetType()

            let htmlOutput =
                if valueType.FullName = "Plotly.NET.GenericChart"
                   || (not (isNull valueType.BaseType) && valueType.BaseType.FullName = "Plotly.NET.GenericChart") then
                    tryInvokeHtmlMethod value
                else
                    tryRenderHtmlContent value

            match htmlOutput with
            | Some value ->
                html value
                true
            | None -> false
"""

    let startInfo =
        ProcessStartInfo(
            FileName = "dotnet",
            Arguments = "fsi --nologo --readline-",
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true,
            WorkingDirectory = workingDirectory
        )

    let proc = Process.Start(startInfo)
    let outputGate = obj ()
    let output = StringBuilder()
    let errors = StringBuilder()

    do
        proc.OutputDataReceived.Add(fun args ->
            if not (isNull args.Data) then
                lock outputGate (fun () -> output.AppendLine(args.Data) |> ignore))

        proc.ErrorDataReceived.Add(fun args ->
            if not (isNull args.Data) then
                lock outputGate (fun () -> errors.AppendLine(args.Data) |> ignore))

        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()

    let snapshotAndClear () =
        lock outputGate (fun () ->
            let outText = output.ToString()
            let errText = errors.ToString()
            output.Clear() |> ignore
            errors.Clear() |> ignore
            outText, errText)

    let cleanOutput (marker: string) (text: string) =
        text.Replace(marker, "").Split([| "\r\n"; "\n"; "\r" |], StringSplitOptions.None)
        |> Array.map (fun line ->
            if line.StartsWith("> ", StringComparison.Ordinal) then
                line.Substring(2)
            else
                line)
        |> Array.filter (fun line ->
            let trimmed = line.Trim()
            trimmed <> ">" && trimmed <> "val it: unit = ()")
        |> String.concat Environment.NewLine
        |> fun value -> value.Trim()

    let mimePayload mimeType (encoded: string) =
        try
            let bytes = Convert.FromBase64String(encoded.Trim())

            match mimeType with
            | MimeTypes.Png -> Some(NotebookOutput.png bytes)
            | MimeTypes.Text ->
                Encoding.UTF8.GetString(bytes)
                |> NotebookOutput.text
                |> Some
            | MimeTypes.Html ->
                Encoding.UTF8.GetString(bytes)
                |> NotebookOutput.html
                |> Some
            | MimeTypes.Svg ->
                Encoding.UTF8.GetString(bytes)
                |> NotebookOutput.svg
                |> Some
            | MimeTypes.PlotlyJson ->
                Encoding.UTF8.GetString(bytes)
                |> NotebookOutput.plotlyJson
                |> Some
            | _ ->
                Some(
                    NotebookOutput.Display
                        { MimeType = mimeType
                          Payload = BinaryPayload bytes }
                )
        with _ ->
            None

    let tryParseMimeEnvelope (text: string) =
        let beginIndex = text.IndexOf(MimeBeginMarker, StringComparison.Ordinal)

        if beginIndex < 0 then
            None
        else
            let mimeStart = beginIndex + MimeBeginMarker.Length
            let newlineIndex = text.IndexOfAny([| '\r'; '\n' |], mimeStart)

            if newlineIndex < 0 then
                None
            else
                let mimeType = text.Substring(mimeStart, newlineIndex - mimeStart).Trim()
                let payloadStart =
                    if newlineIndex + 1 < text.Length && text[newlineIndex] = '\r' && text[newlineIndex + 1] = '\n' then
                        newlineIndex + 2
                    else
                        newlineIndex + 1

                let endIndex = text.IndexOf(MimeEndMarker, payloadStart, StringComparison.Ordinal)

                if endIndex < 0 then
                    None
                else
                    text.Substring(payloadStart, endIndex - payloadStart).Replace("\r", "").Replace("\n", "")
                    |> mimePayload mimeType

    let looksLikeHtml (text: string) =
        let trimmed = text.TrimStart()

        trimmed.StartsWith("<!doctype html", StringComparison.OrdinalIgnoreCase)
        || trimmed.StartsWith("<html", StringComparison.OrdinalIgnoreCase)
        || trimmed.Contains("<script", StringComparison.OrdinalIgnoreCase)

    let inferOutput cleaned =
        match tryParseMimeEnvelope cleaned with
        | Some output -> output
        | None when looksLikeHtml cleaned -> NotebookOutput.html cleaned
        | None -> NotebookOutput.text cleaned

    let hasFsiItValue (text: string) =
        text.Contains("val it:", StringComparison.Ordinal)

    let executeAndCollect (marker: string) (cancellationToken: CancellationToken) =
        task {
            let accumulatedOut = StringBuilder()
            let accumulatedErr = StringBuilder()
            let mutable attempts = 200
            let mutable finished = false

            while not finished && attempts > 0 do
                let outText, errText = snapshotAndClear ()
                accumulatedOut.Append(outText) |> ignore
                accumulatedErr.Append(errText) |> ignore
                finished <- accumulatedOut.ToString().Contains(marker, StringComparison.Ordinal)

                if not finished then
                    attempts <- attempts - 1
                    do! Task.Delay(50, cancellationToken)

            return finished, accumulatedOut.ToString(), accumulatedErr.ToString()
        }

    let markerBinding marker =
        "__lfsx_marker_" + Guid.NewGuid().ToString("N") + " = printfn \"" + marker + "\""

    member _.ExecuteAsync(code: string, cancellationToken: CancellationToken) =
        task {
            if proc.HasExited then
                return
                    { Input = code
                      Output = NotebookOutput.Error "fsi has exited." }
            else
                let marker = "__LFSX_END_" + Guid.NewGuid().ToString("N") + "__"
                let helperMarker = "__LFSX_HELPER_END_" + Guid.NewGuid().ToString("N") + "__"
                snapshotAndClear () |> ignore

                do! proc.StandardInput.WriteLineAsync(displayHelpers)
                do! proc.StandardInput.WriteLineAsync(";;")
                do! proc.StandardInput.WriteLineAsync("let " + markerBinding helperMarker)
                do! proc.StandardInput.WriteLineAsync(";;")
                do! proc.StandardInput.FlushAsync()

                let! _ = executeAndCollect helperMarker cancellationToken
                snapshotAndClear () |> ignore

                do! proc.StandardInput.WriteLineAsync(code)
                do! proc.StandardInput.WriteLineAsync(";;")
                do! proc.StandardInput.WriteLineAsync("let " + markerBinding marker)
                do! proc.StandardInput.WriteLineAsync(";;")
                do! proc.StandardInput.FlushAsync()

                let! finished, outText, errText = executeAndCollect marker cancellationToken

                if finished then
                    let cleaned = cleanOutput marker outText

                    if String.IsNullOrWhiteSpace errText then
                        match tryParseMimeEnvelope cleaned with
                        | None when hasFsiItValue cleaned ->
                            let displayMarker = "__LFSX_DISPLAY_END_" + Guid.NewGuid().ToString("N") + "__"
                            snapshotAndClear () |> ignore
                            do! proc.StandardInput.WriteLineAsync("Lfsx.tryDisplayValue (box it) |> ignore")
                            do! proc.StandardInput.WriteLineAsync(";;")
                            do! proc.StandardInput.WriteLineAsync("let " + markerBinding displayMarker)
                            do! proc.StandardInput.WriteLineAsync(";;")
                            do! proc.StandardInput.FlushAsync()

                            let! displayFinished, displayOut, displayErr =
                                executeAndCollect displayMarker cancellationToken

                            let displayCleaned = cleanOutput displayMarker displayOut

                            if displayFinished && String.IsNullOrWhiteSpace displayErr then
                                match tryParseMimeEnvelope displayCleaned with
                                | Some richOutput ->
                                    return
                                        { Input = code
                                          Output = richOutput }
                                | None ->
                                    return
                                        { Input = code
                                          Output = inferOutput cleaned }
                            else
                                return
                                    { Input = code
                                      Output = inferOutput cleaned }
                        | _ ->
                            return
                                { Input = code
                                  Output = inferOutput cleaned }
                    else
                        return
                            { Input = code
                              Output = NotebookOutput.Error(errText.Trim()) }
                else
                    let text =
                        if String.IsNullOrWhiteSpace errText then
                            outText
                        else
                            errText

                    return
                        { Input = code
                          Output = NotebookOutput.Error("Timed out waiting for fsi output.\n" + text.Trim()) }
        }

    interface IFsiSession with
        member this.ExecuteAsync(code, cancellationToken) =
            this.ExecuteAsync(code, cancellationToken)

    interface IDisposable with
        member _.Dispose() =
            try
                if not proc.HasExited then
                    proc.StandardInput.WriteLine("#quit;;")
                    proc.WaitForExit(1000) |> ignore

                    if not proc.HasExited then
                        proc.Kill(true)
            finally
                proc.Dispose()
