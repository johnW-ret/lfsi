namespace Lfsi.App

open System
open System.IO
open Microsoft.Extensions.Configuration

type FsiExecutable =
    | DotnetOnPath
    | CustomExecutable of string

type FsiConfiguration = { Executable: FsiExecutable }

type FsAutoCompleteConfiguration = { Enabled: bool }

type LfsiConfiguration =
    { Fsi: FsiConfiguration
      FsAutoComplete: FsAutoCompleteConfiguration }

module LfsiConfiguration =
    let private defaultFsi = { Executable = DotnetOnPath }

    let private defaultFsAutoComplete = { Enabled = true }

    let private defaultConfig =
        { Fsi = defaultFsi
          FsAutoComplete = defaultFsAutoComplete }

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

    let private parseEnabled (value: string) =
        match Boolean.TryParse value with
        | true, enabled -> enabled
        | false, _ -> defaultConfig.FsAutoComplete.Enabled

    let load () =
        let configuration =
            ConfigurationBuilder()
                .SetBasePath(Environment.CurrentDirectory)
                .AddJsonFile(Path.Combine(".config", "lfsi.json"), optional = true, reloadOnChange = false)
                .AddJsonFile("lfsi.json", optional = true, reloadOnChange = false)
                .Build()

        { Fsi = { Executable = parseFsiExecutable configuration["Fsi:ExecutablePath"] }
          FsAutoComplete = { Enabled = parseEnabled configuration["FsAutoComplete:Enabled"] } }
