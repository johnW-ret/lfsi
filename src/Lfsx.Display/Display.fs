namespace Lfsx

open System
open System.IO
open System.Reflection
open System.Text
open System.Threading.Tasks
open Microsoft.DotNet.Interactive.Formatting

module Display =

    // ── Output helpers ──────────────────────────────────────────────

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

    // ── Rich-value formatting ───────────────────────────────────────

    let private tryFormatHtml (value: obj) =
        try Some(Formatter.ToDisplayString(value, "text/html")) with _ -> None

    // ── Kernel extension loading ────────────────────────────────────

    let private extensionInterfaceName = "Microsoft.DotNet.Interactive.IKernelExtension"
    let private onLoadAsyncName = "Microsoft.DotNet.Interactive.IKernelExtension.OnLoadAsync"

    let private tryLoadExtension (t: Type) =
        let iface = t.GetInterface(extensionInterfaceName)
        if not (isNull iface) then
            let onLoadAsync = t.GetMethod(onLoadAsyncName, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            if not (isNull onLoadAsync) then
                let ext = Activator.CreateInstance(t)
                let task = onLoadAsync.Invoke(ext, [| null |]) :?> Task
                task.GetAwaiter().GetResult()

    let private tryLoadExtensionsFrom (asm: Assembly) =
        try
            for t in asm.GetExportedTypes() do
                tryLoadExtension t
        with _ ->
            ()

    let private tryLoadExtensions () =
        for asm in AppDomain.CurrentDomain.GetAssemblies() do
            tryLoadExtensionsFrom asm

        for asm in AppDomain.CurrentDomain.GetAssemblies() do
            let name = asm.GetName().Name
            if not (isNull name) then
                for candidateName in [ name + ".Interactive"; name + ".DotNetInteractive" ] do
                    try
                        let candidateAsm = Assembly.Load(candidateName)
                        tryLoadExtensionsFrom candidateAsm
                    with _ ->
                        ()

    let private loadExtensions = lazy tryLoadExtensions ()

    // ── Public entry point ──────────────────────────────────────────

    let tryDisplayValue (value: obj) =
        if isNull value then
            false
        else
            loadExtensions.Force()

            match tryFormatHtml value with
            | Some value ->
                html value
                true
            | None -> false
