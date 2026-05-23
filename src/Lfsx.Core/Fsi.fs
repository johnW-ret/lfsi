namespace Lfsx.Core

open System
open System.Diagnostics
open System.IO
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
        let escapedAssemblyPath (path: string) =
            path.Replace("\\", "\\\\").Replace("\"", "\\\"")

        let dllPath =
            Path.Combine(Path.GetDirectoryName(typeof<FsiSession>.Assembly.Location), "Lfsx.Display.dll")
            |> escapedAssemblyPath

        "#r \"" + dllPath + "\"" + Environment.NewLine + "open Lfsx"

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

    let proc = Process.Start startInfo
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
            let decodeText () = Encoding.UTF8.GetString(bytes)

            match mimeType with
            | MimeTypes.Png -> NotebookOutput.png bytes
            | MimeTypes.Text -> NotebookOutput.text (decodeText ())
            | MimeTypes.Html -> NotebookOutput.html (decodeText ())
            | MimeTypes.Svg -> NotebookOutput.svg (decodeText ())
            | _ ->
                NotebookOutput.Display
                    { MimeType = mimeType
                      Payload = BinaryPayload bytes }
            |> Some
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
                    if
                        newlineIndex + 1 < text.Length
                        && text[newlineIndex] = '\r'
                        && text[newlineIndex + 1] = '\n'
                    then
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
                accumulatedOut.Append outText |> ignore
                accumulatedErr.Append errText |> ignore
                finished <- accumulatedOut.ToString().Contains(marker, StringComparison.Ordinal)

                if not finished then
                    attempts <- attempts - 1
                    do! Task.Delay(50, cancellationToken)

            return finished, accumulatedOut.ToString(), accumulatedErr.ToString()
        }

    let markerBinding marker =
        "__lfsx_marker_" + Guid.NewGuid().ToString "N" + " = printfn \"" + marker + "\""

    let sendCodeAndCollect (code: string) (marker: string) (cancellationToken: CancellationToken) =
        task {
            do! proc.StandardInput.WriteLineAsync code
            do! proc.StandardInput.WriteLineAsync ";;"
            do! proc.StandardInput.WriteLineAsync("let " + markerBinding marker)
            do! proc.StandardInput.WriteLineAsync ";;"
            do! proc.StandardInput.FlushAsync()
            return! executeAndCollect marker cancellationToken
        }

    let tryDisplayValue (code: string) (cleaned: string) (cancellationToken: CancellationToken) =
        task {
            let displayMarker = "__LFSX_DISPLAY_END_" + Guid.NewGuid().ToString "N" + "__"
            snapshotAndClear () |> ignore

            let! displayFinished, displayOut, displayErr =
                sendCodeAndCollect "Display.tryDisplayValue (box it) |> ignore" displayMarker cancellationToken

            let displayCleaned = cleanOutput displayMarker displayOut

            if displayFinished && String.IsNullOrWhiteSpace displayErr then
                match tryParseMimeEnvelope displayCleaned with
                | Some richOutput -> return { Input = code; Output = richOutput }
                | None ->
                    return
                        { Input = code
                          Output = inferOutput cleaned }
            else
                return
                    { Input = code
                      Output = inferOutput cleaned }
        }

    member _.ExecuteAsync(code: string, cancellationToken: CancellationToken) =
        task {
            if proc.HasExited then
                return
                    { Input = code
                      Output = NotebookOutput.Error "fsi has exited." }
            else
                let marker = "__LFSX_END_" + Guid.NewGuid().ToString "N" + "__"
                let helperMarker = "__LFSX_HELPER_END_" + Guid.NewGuid().ToString "N" + "__"
                snapshotAndClear () |> ignore

                let! helperFinished, _, helperErr = sendCodeAndCollect displayHelpers helperMarker cancellationToken
                snapshotAndClear () |> ignore

                if not helperFinished || not (String.IsNullOrWhiteSpace helperErr) then
                    let text =
                        if String.IsNullOrWhiteSpace helperErr then
                            "Timed out waiting for fsi display helper."
                        else
                            helperErr.Trim()

                    return
                        { Input = code
                          Output = NotebookOutput.Error("Failed to initialize fsi display helper.\n" + text) }
                else
                    let! finished, outText, errText = sendCodeAndCollect code marker cancellationToken

                    if finished then
                        let cleaned = cleanOutput marker outText

                        if String.IsNullOrWhiteSpace errText then
                            match tryParseMimeEnvelope cleaned with
                            | None when hasFsiItValue cleaned -> return! tryDisplayValue code cleaned cancellationToken
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
