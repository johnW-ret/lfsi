namespace Lfsi.App

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open StreamJsonRpc

type CompletionContext = { Source: string; CursorOffset: int }

type CompletionItem =
    { Label: string
      InsertText: string
      Detail: string option }

type ICompletionService =
    inherit IDisposable

    abstract CompleteAsync: CompletionContext * CancellationToken -> Task<CompletionItem list>

type DisabledCompletionService() =
    interface ICompletionService with
        member _.CompleteAsync(_, _) = Task.FromResult []
        member _.Dispose() = ()

type FsAutoCompleteCompletionService(workingDirectory: string, documentPath: string option) =
    let gate = new SemaphoreSlim(1, 1)
    let logPath = Environment.GetEnvironmentVariable "LFSI_FSAC_LOG" |> Option.ofObj
    let mutable serverProcess: Process option = None
    let mutable rpc: JsonRpc option = None
    let mutable initialized = false
    let mutable documentOpened = false
    let mutable version = 0

    let documentUri =
        documentPath
        |> Option.defaultValue (Path.Combine(workingDirectory, ".lfsi-untitled.fsx"))
        |> Path.GetFullPath
        |> Uri
        |> string

    let positionAt offset (source: string) =
        let offset = Math.Clamp(offset, 0, source.Length)
        let beforeCursor = source.AsSpan(0, offset)
        let mutable line = 0
        let mutable character = 0

        for value in beforeCursor do
            if value = '\n' then
                line <- line + 1
                character <- 0
            else
                character <- character + 1

        {| line = line; character = character |}

    let resolveExecutable () =
        let executableName =
            if OperatingSystem.IsWindows() then
                "fsautocomplete.exe"
            else
                "fsautocomplete"

        let pathCandidates =
            Environment.GetEnvironmentVariable "PATH"
            |> Option.ofObj
            |> Option.defaultValue ""
            |> fun value -> value.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun directory -> Path.Combine(directory, executableName))

        let globalToolCandidate =
            Path.Combine(
                Environment.GetFolderPath Environment.SpecialFolder.UserProfile,
                ".dotnet",
                "tools",
                executableName
            )

        Array.append pathCandidates [| globalToolCandidate |]
        |> Array.tryFind File.Exists
        |> Option.defaultValue executableName

    let startServer () =
        let startInfo =
            ProcessStartInfo(
                FileName = resolveExecutable (),
                WorkingDirectory = workingDirectory,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true
            )

        startInfo.ArgumentList.Add("--adaptive-lsp-server-enabled")

        logPath
        |> Option.iter (fun path ->
            startInfo.ArgumentList.Add("--log-file")
            startInfo.ArgumentList.Add(path))

        let server = new Process(StartInfo = startInfo)

        if not (server.Start()) then
            failwith "fsautocomplete did not start."

        server.ErrorDataReceived.Add(fun _ -> ())
        server.BeginErrorReadLine()

        let handler =
            new HeaderDelimitedMessageHandler(
                server.StandardInput.BaseStream,
                server.StandardOutput.BaseStream,
                new JsonMessageFormatter()
            )

        let connection = new JsonRpc(handler)
        connection.StartListening()
        serverProcess <- Some server
        rpc <- Some connection
        connection

    let initialize (connection: JsonRpc) cancellationToken =
        task {
            if not initialized then
                let rootUri = Uri(Path.GetFullPath workingDirectory) |> string

                let parameters =
                    {| processId = Environment.ProcessId
                       rootUri = rootUri
                       capabilities =
                        {| textDocument =
                            {| completion =
                                {| completionItem =
                                    {| snippetSupport = false
                                       documentationFormat = [| "plaintext" |] |} |} |} |}
                       initializationOptions = {| AutomaticWorkspaceInit = false |}
                       workspaceFolders =
                        [| {| uri = rootUri
                              name = Path.GetFileName workingDirectory |} |] |}

                let! _ =
                    connection.InvokeWithCancellationAsync<JToken>(
                        "initialize",
                        [| box parameters |],
                        cancellationToken
                    )

                do! connection.NotifyAsync("initialized", box {| |})
                initialized <- true
        }

    let synchronizeDocument (connection: JsonRpc) source =
        task {
            version <- version + 1

            if documentOpened then
                do!
                    connection.NotifyAsync(
                        "textDocument/didChange",
                        box
                            {| textDocument =
                                {| uri = documentUri
                                   version = version |}
                               contentChanges = [| {| text = source |} |] |}
                    )
            else
                do!
                    connection.NotifyAsync(
                        "textDocument/didOpen",
                        box
                            {| textDocument =
                                {| uri = documentUri
                                   languageId = "fsharp"
                                   version = version
                                   text = source |} |}
                    )

                documentOpened <- true
        }

    let parseCompletionItem (item: JToken) =
        let stringValue name =
            item.[name]
            |> Option.ofObj
            |> Option.bind (fun value ->
                let text = value.Value<string>()
                if String.IsNullOrWhiteSpace text then None else Some text)

        let label = stringValue "label" |> Option.defaultValue ""

        let insertText =
            item.SelectToken("textEdit.newText")
            |> Option.ofObj
            |> Option.bind (fun value -> value.Value<string>() |> Option.ofObj)
            |> Option.orElseWith (fun () -> stringValue "insertText")
            |> Option.defaultValue label

        if String.IsNullOrWhiteSpace label || insertText.Contains("$") then
            None
        else
            Some
                { Label = label
                  InsertText = insertText
                  Detail = stringValue "detail" }

    let parseCompletionResult (result: JToken) =
        let items =
            match result with
            | :? JArray as array -> array :> seq<JToken>
            | _ ->
                result.["items"]
                |> Option.ofObj
                |> Option.map (fun value -> value.Children() |> Seq.cast<JToken>)
                |> Option.defaultValue Seq.empty

        items |> Seq.choose parseCompletionItem |> Seq.truncate 50 |> List.ofSeq

    interface ICompletionService with
        member _.CompleteAsync(context, cancellationToken) =
            task {
                try
                    do! gate.WaitAsync(cancellationToken)

                    try
                        let connection = rpc |> Option.defaultWith startServer
                        do! initialize connection cancellationToken
                        do! synchronizeDocument connection context.Source

                        let parameters =
                            {| textDocument = {| uri = documentUri |}
                               position = positionAt context.CursorOffset context.Source
                               context = {| triggerKind = 1 |} |}

                        let! result =
                            connection.InvokeWithCancellationAsync<JToken>(
                                "textDocument/completion",
                                [| box parameters |],
                                cancellationToken
                            )

                        return parseCompletionResult result
                    finally
                        gate.Release() |> ignore
                with
                | :? OperationCanceledException -> return []
                | ex ->
                    logPath |> Option.iter (fun path -> File.AppendAllText(path, $"\nlfsi: {ex}\n"))
                    return []
            }

    interface IDisposable with
        member _.Dispose() =
            rpc |> Option.iter _.Dispose()

            serverProcess
            |> Option.iter (fun server ->
                if not server.HasExited then
                    server.Kill(true)

                server.Dispose())

            gate.Dispose()
