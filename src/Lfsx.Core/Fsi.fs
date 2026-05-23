namespace Lfsx.Core

open System
open System.Diagnostics
open System.Text
open System.Threading
open System.Threading.Tasks
open Microsoft.DotNet.Interactive.Formatting

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

    let escapedAssemblyPath (path: string) =
        path.Replace("\\", "\\\\").Replace("\"", "\\\"")

    let displayHelpers =
        let formattingAssemblyPath =
            typeof<Formatter>.Assembly.Location
            |> escapedAssemblyPath

        String.concat
            Environment.NewLine
            [ "#r \"" + formattingAssemblyPath + "\""
              ""
              """
module Lfsx =
    open System
    open System.IO
    open System.Reflection
    open System.Text
    open Microsoft.DotNet.Interactive.Formatting

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

    let private isRelevantFormatterType (valueType: Type) (formatterType: Type) =
        formatterType.IsAssignableFrom(valueType)

    let private tryFormatWithFormatterObject (value: obj) (formatter: obj) =
        try
            let formatterType = formatter.GetType()
            let mimeType =
                match formatterType.GetProperty("MimeType") with
                | null -> None
                | property ->
                    match property.GetValue(formatter) with
                    | :? string as value -> Some value
                    | _ -> None

            if mimeType <> Some "text/html" then
                None
            else
                let valueType = value.GetType()
                let formatterTargetType =
                    match formatterType.GetProperty("Type") with
                    | null -> None
                    | property ->
                        match property.GetValue(formatter) with
                        | :? Type as value -> Some value
                        | _ -> None

                match formatterTargetType with
                | Some targetType when not (isRelevantFormatterType valueType targetType) -> None
                | _ ->
                    let formatMethod =
                        formatterType.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
                        |> Array.tryFind (fun method ->
                            method.Name = "Format"
                            && method.GetParameters().Length = 2
                            && method.GetParameters().[0].ParameterType.IsAssignableFrom(valueType)
                            && method.GetParameters().[1].ParameterType.IsAssignableFrom(typeof<TextWriter>))

                    match formatMethod with
                    | Some method ->
                        use writer = new StringWriter()

                        match method.Invoke(formatter, [| value; writer :> obj |]) with
                        | :? bool as handled when handled -> Some(writer.ToString())
                        | null -> Some(writer.ToString())
                        | _ -> None
                    | None -> None
        with _ ->
            None

    let private typeHierarchy (valueType: Type) =
        Seq.unfold
            (fun (current: Type) ->
                if isNull current then
                    None
                else
                    Some(current, current.BaseType))
            valueType

    let private formatterSources (valueType: Type) =
        typeHierarchy valueType
        |> Seq.collect (fun candidateType -> candidateType.GetCustomAttributes(true))
        |> Seq.choose (fun attribute ->
            let attributeType = attribute.GetType()

            if attributeType.FullName = "Microsoft.DotNet.Interactive.Formatting.TypeFormatterSourceAttribute"
               || attributeType.Name = "TypeFormatterSourceAttribute" then
                match attributeType.GetProperty("FormatterSourceType") with
                | null -> None
                | property ->
                    match property.GetValue(attribute) with
                    | :? Type as formatterSourceType -> Some formatterSourceType
                    | _ -> None
            else
                None)

    let private tryFormatWithFormatterSources (value: obj) =
        let valueType = value.GetType()

        formatterSources valueType
        |> Seq.tryPick (fun formatterSourceType ->
            try
                let source = Activator.CreateInstance(formatterSourceType)
                let createFormatters = formatterSourceType.GetMethod("CreateTypeFormatters")

                if isNull createFormatters then
                    None
                else
                    match createFormatters.Invoke(source, Array.empty) with
                    | :? System.Collections.IEnumerable as formatters ->
                        formatters
                        |> Seq.cast<obj>
                        |> Seq.tryPick (tryFormatWithFormatterObject value)
                    | _ -> None
            with _ ->
                None)

    let private tryFormatWithRegisteredFormatter (value: obj) =
        let valueType = value.GetType()

        try
            Formatter.GetPreferredMimeTypesFor(valueType) |> ignore

            Formatter.RegisteredFormatters(false)
            |> Seq.tryPick (fun formatter ->
                let handlesValue =
                    formatter.MimeType = "text/html"
                    && formatter.Type.IsAssignableFrom(valueType)

                if handlesValue then
                    try
                        Some(Formatter.ToDisplayString(value, "text/html"))
                    with _ ->
                        None
                else
                    None)
        with _ ->
            None

    let private tryLoadExtensions () =
        let extensionInterfaceName = "Microsoft.DotNet.Interactive.IKernelExtension"
        let onLoadAsyncName = "Microsoft.DotNet.Interactive.IKernelExtension.OnLoadAsync"

        for asm in System.AppDomain.CurrentDomain.GetAssemblies() do
            try
                for t in asm.GetExportedTypes() do
                    let iface = t.GetInterface(extensionInterfaceName)
                    if not (isNull iface) then
                        let onLoadAsync = t.GetMethod(onLoadAsyncName, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                        if not (isNull onLoadAsync) then
                            let ext = System.Activator.CreateInstance(t)
                            let task = onLoadAsync.Invoke(ext, [| null |]) :?> System.Threading.Tasks.Task
                            task.Wait()
            with _ ->
                ()

        for asm in System.AppDomain.CurrentDomain.GetAssemblies() do
            let name = asm.GetName().Name
            if not (isNull name) then
                for candidateName in [ name + ".Interactive"; name + ".DotNetInteractive" ] do
                    try
                        let candidateAsm = System.Reflection.Assembly.Load(candidateName)
                        for t in candidateAsm.GetExportedTypes() do
                            let iface = t.GetInterface(extensionInterfaceName)
                            if not (isNull iface) then
                                let onLoadAsync = t.GetMethod(onLoadAsyncName, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                                if not (isNull onLoadAsync) then
                                    let ext = System.Activator.CreateInstance(t)
                                    let task = onLoadAsync.Invoke(ext, [| null |]) :?> System.Threading.Tasks.Task
                                    task.Wait()
                    with _ ->
                        ()

    let mutable private _extensionsLoaded = false

    let private tryFormatHtml (value: obj) =
        if not _extensionsLoaded then
            _extensionsLoaded <- true
            tryLoadExtensions ()

        tryFormatWithRegisteredFormatter value
        |> Option.orElseWith (fun () -> tryFormatWithFormatterSources value)

    let tryDisplayValue (value: obj) =
        if isNull value then
            false
        else
            match tryFormatHtml value with
            | Some value ->
                html value
                true
            | None -> false
""" ]

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

                let! helperFinished, _, helperErr = executeAndCollect helperMarker cancellationToken
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
