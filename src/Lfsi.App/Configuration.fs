namespace Lfsi.App

open System
open System.IO
open Microsoft.Extensions.Configuration

type FsiConfiguration = { ExecutablePath: string }

type LfsiConfiguration = { Fsi: FsiConfiguration }

module LfsiConfiguration =
    let private defaultFsi = { ExecutablePath = "dotnet" }

    let private defaultConfig = { Fsi = defaultFsi }

    let load () =
        let configuration =
            ConfigurationBuilder()
                .SetBasePath(Environment.CurrentDirectory)
                .AddJsonFile(Path.Combine(".config", "lfsi.json"), optional = true, reloadOnChange = false)
                .AddJsonFile("lfsi.json", optional = true, reloadOnChange = false)
                .Build()

        { Fsi =
            { ExecutablePath =
                configuration["Fsi:ExecutablePath"]
                |> Option.ofObj
                |> Option.filter (String.IsNullOrWhiteSpace >> not)
                |> Option.defaultValue defaultConfig.Fsi.ExecutablePath } }
