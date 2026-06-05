namespace Lfsi.App

open System
open System.IO
open Microsoft.Extensions.Configuration

type FsiExecutable =
    | DotnetOnPath
    | CustomExecutable of string

type FsiConfiguration = { Executable: FsiExecutable }

type LfsiConfiguration = { Fsi: FsiConfiguration }

module LfsiConfiguration =
    let private defaultFsi = { Executable = DotnetOnPath }

    let private defaultConfig = { Fsi = defaultFsi }

    let private parseFsiExecutable value =
        value
        |> Option.ofObj
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        |> Option.map CustomExecutable
        |> Option.defaultValue defaultConfig.Fsi.Executable

    let fsiExecutablePath executable =
        match executable with
        | DotnetOnPath -> "dotnet"
        | CustomExecutable path -> path

    let load () =
        let configuration =
            ConfigurationBuilder()
                .SetBasePath(Environment.CurrentDirectory)
                .AddJsonFile(Path.Combine(".config", "lfsi.json"), optional = true, reloadOnChange = false)
                .AddJsonFile("lfsi.json", optional = true, reloadOnChange = false)
                .Build()

        { Fsi = { Executable = parseFsiExecutable configuration["Fsi:ExecutablePath"] } }
