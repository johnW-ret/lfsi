namespace Lfsi.App

open System

type TerminalGraphicsProtocol =
    | Kitty
    | Sixel
    | Osc1337

type TerminalGraphicsMode =
    | Disabled
    | Auto
    | Force of TerminalGraphicsProtocol

type TerminalClient =
    | KittyTerminal
    | Ghostty
    | WezTerm
    | ITerm2
    | WindowsTerminal
    | UnknownTerminal

type TerminalEnvironment =
    { Term: string option
      TermProgram: string option
      WtSession: string option
      GhosttyResourcesDir: string option
      LfsiTerminalGraphics: string option
      LfsiEnableKittyGraphics: string option }

type TerminalGraphicsDecision =
    | UseTerminalGraphics of TerminalGraphicsProtocol
    | UseTextFallback of string

module TerminalGraphics =
    let private env name =
        match Environment.GetEnvironmentVariable name with
        | value when String.IsNullOrWhiteSpace value -> None
        | value -> Some value

    let currentEnvironment () =
        { Term = env "TERM"
          TermProgram = env "TERM_PROGRAM"
          WtSession = env "WT_SESSION"
          GhosttyResourcesDir = env "GHOSTTY_RESOURCES_DIR"
          LfsiTerminalGraphics = env "LFSI_TERMINAL_GRAPHICS"
          LfsiEnableKittyGraphics = env "LFSI_ENABLE_KITTY_GRAPHICS" }

    let private contains (part: string) (value: string option) =
        value
        |> Option.exists (fun text -> text.Contains(part, StringComparison.OrdinalIgnoreCase))

    let detectClient environment =
        if
            contains "xterm-kitty" environment.Term
            || contains "kitty" environment.TermProgram
        then
            KittyTerminal
        elif
            contains "xterm-ghostty" environment.Term
            || contains "ghostty" environment.TermProgram
            || environment.GhosttyResourcesDir.IsSome
        then
            Ghostty
        elif contains "wezterm" environment.TermProgram then
            WezTerm
        elif contains "iTerm.app" environment.TermProgram then
            ITerm2
        elif environment.WtSession.IsSome then
            WindowsTerminal
        else
            UnknownTerminal

    let parseMode environment =
        match
            environment.LfsiTerminalGraphics
            |> Option.map (fun value -> value.Trim().ToLowerInvariant())
        with
        | Some "off"
        | Some "false"
        | Some "0"
        | Some "none"
        | Some "disabled" -> Disabled
        | Some "kitty" -> Force Kitty
        | Some "sixel" -> Force Sixel
        | Some "osc1337"
        | Some "iterm2" -> Force Osc1337
        | Some "auto"
        | None ->
            match environment.LfsiEnableKittyGraphics with
            | Some "1" -> Force Kitty
            | _ -> Auto
        | Some _ -> Auto

    let decide environment =
        match parseMode environment with
        | Disabled -> UseTextFallback "Terminal graphics are disabled."
        | Force protocol -> UseTerminalGraphics protocol
        | Auto ->
            match detectClient environment with
            | KittyTerminal
            | Ghostty -> UseTerminalGraphics Kitty
            | WezTerm ->
                UseTextFallback "WezTerm terminal graphics require explicit opt-in with LFSI_TERMINAL_GRAPHICS=kitty."
            | ITerm2 -> UseTextFallback "iTerm2 graphics require the OSC1337 backend."
            | WindowsTerminal -> UseTerminalGraphics Sixel
            | UnknownTerminal -> UseTextFallback "No supported terminal graphics protocol was detected."
