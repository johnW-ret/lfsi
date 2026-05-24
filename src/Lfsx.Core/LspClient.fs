namespace Lfsx.Core

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks

type CompletionItem =
    { Label: string
      InsertText: string option
      Detail: string option
      Kind: int option }

type LspClient(workingDirectory: string, fsAutocompleteDll: string) =
    let rootUri =
        let dir = Path.GetFullPath(workingDirectory).TrimEnd('/')
        let uri = Uri(dir + "/")
        uri.AbsoluteUri.TrimEnd('/')

    let documentUri = rootUri + "/cell.fsx"

    let jsonOptions =
        JsonSerializerOptions(
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
        )

    let pendingRequests = ConcurrentDictionary<int, TaskCompletionSource<JsonElement>>()
    let cts = new CancellationTokenSource()
    let mutable nextId = 1
    let mutable proc: Process = null
    let mutable stdinWriter: StreamWriter = null
    let mutable stdoutStream: Stream = null
    let writeLock = obj ()
    let initCompleted = TaskCompletionSource<unit>()
    let mutable documentVersion = 1

    let getNextId () = Interlocked.Increment &nextId

    let sendMessage (json: string) =
        let bytes = Encoding.UTF8.GetBytes(json)
        let header = sprintf "Content-Length: %d\r\n\r\n" bytes.Length

        lock writeLock (fun () ->
            if not (isNull stdinWriter) && not (isNull proc) && not proc.HasExited then
                try
                    stdinWriter.Write(header)
                    stdinWriter.Write(json)
                    stdinWriter.Flush()
                with _ ->
                    ())

    let sendRequest (method: string) (parameters: obj) =
        task {
            let id = getNextId ()
            let tcs = TaskCompletionSource<JsonElement>()
            pendingRequests.TryAdd(id, tcs) |> ignore

            let msg =
                JsonSerializer.SerializeToElement(
                    {| jsonrpc = "2.0"
                       id = id
                       ``method`` = method
                       ``params`` = parameters |},
                    jsonOptions
                )

            sendMessage (msg.GetRawText())

            try
                let! result = tcs.Task.WaitAsync(TimeSpan.FromSeconds 15.0)

                if
                    result.ValueKind = JsonValueKind.Undefined
                    || result.ValueKind = JsonValueKind.Null
                then
                    return ValueNone
                else
                    return ValueSome result
            with :? TimeoutException ->
                pendingRequests.TryRemove(id) |> ignore
                return ValueNone
        }

    let sendNotification (method: string) (parameters: obj) =
        let msg =
            JsonSerializer.SerializeToElement(
                {| jsonrpc = "2.0"
                   ``method`` = method
                   ``params`` = parameters |},
                jsonOptions
            )

        sendMessage (msg.GetRawText())

    let readStreamLine (stream: Stream) (ct: CancellationToken) =
        task {
            let sb = StringBuilder()
            let mutable finished = false

            while not finished && not ct.IsCancellationRequested do
                let buffer = Array.zeroCreate<byte> 1
                let! read = stream.ReadAsync(buffer, 0, 1, ct)

                if read = 0 then
                    finished <- true
                else
                    let b = int buffer[0]

                    if b = 10 then // \n
                        finished <- true
                    elif b <> 13 then // skip \r
                        sb.Append(char b) |> ignore

            if ct.IsCancellationRequested then return null
            elif sb.Length = 0 && finished then return null
            else return sb.ToString()
        }

    let readBodyChunk (stream: Stream) (buffer: byte[]) (offset: int) (count: int) (ct: CancellationToken) =
        task {
            let mutable totalRead = 0
            let mutable finished = false

            while not finished && totalRead < count && not ct.IsCancellationRequested do
                let! read = stream.ReadAsync(buffer, offset + totalRead, count - totalRead, ct)

                if read = 0 then
                    finished <- true
                else
                    totalRead <- totalRead + read

            return totalRead
        }

    let dispatchMessage (json: string) =
        try
            use doc = JsonDocument.Parse(json)
            let root = doc.RootElement
            let mutable idProp = JsonElement()

            if root.TryGetProperty("id", &idProp) then
                let id = idProp.GetInt32()

                match pendingRequests.TryRemove(id) with
                | true, tcs ->
                    let mutable errorProp = JsonElement()

                    if root.TryGetProperty("error", &errorProp) then
                        let mutable msgProp = JsonElement()

                        let msg =
                            if errorProp.TryGetProperty("message", &msgProp) then
                                msgProp.GetString()
                            else
                                "LSP error"

                        tcs.TrySetException(Exception(msg)) |> ignore
                    else
                        let mutable resultProp = JsonElement()

                        if root.TryGetProperty("result", &resultProp) then
                            tcs.TrySetResult(resultProp) |> ignore
                        else
                            tcs.TrySetResult(JsonSerializer.SerializeToElement(null, jsonOptions)) |> ignore
                | _ -> ()
        with _ ->
            ()

    let runMessageLoop () =
        task {
            let stream = stdoutStream

            try
                while not cts.IsCancellationRequested do
                    let! headerLine = readStreamLine stream cts.Token

                    if isNull headerLine then
                        ()
                    elif headerLine.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase) then
                        let length =
                            headerLine.AsSpan("Content-Length:".Length).Trim().ToString() |> Int32.Parse

                        let! _blank = readStreamLine stream cts.Token

                        if not cts.IsCancellationRequested then
                            let buffer = Array.zeroCreate<byte> length
                            let! totalRead = readBodyChunk stream buffer 0 length cts.Token

                            if totalRead > 0 then
                                let json = Encoding.UTF8.GetString(buffer, 0, totalRead)
                                dispatchMessage json
            with _ ->
                ()
        }

    member this.StartAsync() =
        task {
            try
                let startInfo =
                    ProcessStartInfo(
                        FileName = "dotnet",
                        Arguments = sprintf "exec \"%s\"" fsAutocompleteDll,
                        RedirectStandardInput = true,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false,
                        CreateNoWindow = true,
                        WorkingDirectory = workingDirectory
                    )

                proc <- Process.Start(startInfo)
                stdinWriter <- proc.StandardInput
                stdoutStream <- proc.StandardOutput.BaseStream
                // discard stderr to avoid polluting the terminal
                proc.ErrorDataReceived.Add(fun _ -> ())
                proc.BeginErrorReadLine() |> ignore
                let _readerTask = runMessageLoop ()

                let! _initResult =
                    sendRequest
                        "initialize"
                        {| processId = Nullable()
                           rootUri = rootUri
                           capabilities =
                            {| textDocument =
                                {| completion = {| |}
                                   synchronization = {| didSave = false |} |} |} |}

                sendNotification "initialized" null
            finally
                initCompleted.TrySetResult() |> ignore
        }

    member this.WaitForInitAsync() = initCompleted.Task

    member this.OpenDocument(text: string) =
        documentVersion <- 1

        sendNotification
            "textDocument/didOpen"
            {| textDocument =
                {| uri = documentUri
                   languageId = "fsharp"
                   version = documentVersion
                   text = text |} |}

    member this.ChangeDocument(text: string) =
        documentVersion <- documentVersion + 1

        sendNotification
            "textDocument/didChange"
            {| textDocument =
                {| uri = documentUri
                   version = documentVersion |}
               contentChanges = [| {| text = text |} |] |}

    member this.RequestCompletionsAsync(text: string, cursorPosition: int) =
        task {
            let effectivePos = min cursorPosition text.Length
            let textBefore = text.Substring(0, effectivePos)
            let lines = textBefore.Split('\n')
            let line = lines.Length - 1
            let lastNewline = textBefore.LastIndexOf('\n')

            let character =
                if lastNewline >= 0 then
                    effectivePos - lastNewline - 1
                else
                    effectivePos

            let triggerChar =
                if character > 0 && effectivePos <= text.Length then
                    Some text[effectivePos - 1]
                else
                    None

            let triggerKind, triggerCharacterParam =
                match triggerChar with
                | Some ch when ch = '.' || ch = '\'' -> 2, string ch
                | _ -> 1, null

            let! result =
                sendRequest
                    "textDocument/completion"
                    {| textDocument = {| uri = documentUri |}
                       position = {| line = line; character = character |}
                       context =
                        {| triggerKind = triggerKind
                           triggerCharacter = triggerCharacterParam |} |}

            match result with
            | ValueNone -> return [||]
            | ValueSome json ->
                let items =
                    if json.ValueKind = JsonValueKind.Array then
                        json.EnumerateArray()
                        |> Seq.map (fun item ->
                            let label = item.GetProperty("label").GetString()

                            let tryGetString (name: string) =
                                let mutable prop = JsonElement()

                                if item.TryGetProperty(name, &prop) then
                                    Some(prop.GetString())
                                else
                                    None

                            let tryGetInt (name: string) =
                                let mutable prop = JsonElement()

                                if item.TryGetProperty(name, &prop) then
                                    Some(prop.GetInt32())
                                else
                                    None

                            { Label = label
                              InsertText = tryGetString "insertText"
                              Detail = tryGetString "detail"
                              Kind = tryGetInt "kind" })
                        |> Seq.toArray
                    else
                        let mutable itemsProp = JsonElement()

                        if json.TryGetProperty("items", &itemsProp) then
                            itemsProp.EnumerateArray()
                            |> Seq.map (fun item ->
                                let label = item.GetProperty("label").GetString()

                                let tryGetString (name: string) =
                                    let mutable prop = JsonElement()

                                    if item.TryGetProperty(name, &prop) then
                                        Some(prop.GetString())
                                    else
                                        None

                                let tryGetInt (name: string) =
                                    let mutable prop = JsonElement()

                                    if item.TryGetProperty(name, &prop) then
                                        Some(prop.GetInt32())
                                    else
                                        None

                                { Label = label
                                  InsertText = tryGetString "insertText"
                                  Detail = tryGetString "detail"
                                  Kind = tryGetInt "kind" })
                            |> Seq.toArray
                        else
                            [||]

                return items
        }

    interface IDisposable with
        member this.Dispose() =
            cts.Cancel()

            if not (isNull proc) then
                try
                    sendRequest "shutdown" null |> ignore
                    proc.WaitForExit(2000) |> ignore

                    if not proc.HasExited then
                        proc.Kill(true)
                with _ ->
                    if not proc.HasExited then
                        proc.Kill(true)

                proc.Dispose()

            cts.Dispose()
