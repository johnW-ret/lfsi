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
    let workingDirectory = defaultArg workingDirectory Environment.CurrentDirectory

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

    member _.ExecuteAsync(code: string, cancellationToken: CancellationToken) =
        task {
            if proc.HasExited then
                return
                    { Input = code
                      Output = NotebookOutput.Error "fsi has exited." }
            else
                let marker = "__LFSX_END_" + Guid.NewGuid().ToString("N") + "__"
                snapshotAndClear () |> ignore

                do! proc.StandardInput.WriteLineAsync(code)
                do! proc.StandardInput.WriteLineAsync(";;")
                do! proc.StandardInput.WriteLineAsync("printfn \"" + marker + "\"")
                do! proc.StandardInput.WriteLineAsync(";;")
                do! proc.StandardInput.FlushAsync()

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

                let outText = accumulatedOut.ToString()
                let errText = accumulatedErr.ToString()

                if finished then
                    let cleaned = cleanOutput marker outText

                    if String.IsNullOrWhiteSpace errText then
                        return
                            { Input = code
                              Output = NotebookOutput.Text cleaned }
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
