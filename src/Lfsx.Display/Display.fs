module Lfsx

open System
open System.IO
open System.Reflection
open System.Text
open System.Threading.Tasks
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

    for asm in AppDomain.CurrentDomain.GetAssemblies() do
        try
            for t in asm.GetExportedTypes() do
                let iface = t.GetInterface(extensionInterfaceName)
                if not (isNull iface) then
                    let onLoadAsync = t.GetMethod(onLoadAsyncName, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                    if not (isNull onLoadAsync) then
                        let ext = Activator.CreateInstance(t)
                        let task = onLoadAsync.Invoke(ext, [| null |]) :?> Task
                        task.Wait()
        with _ ->
            ()

    for asm in AppDomain.CurrentDomain.GetAssemblies() do
        let name = asm.GetName().Name
        if not (isNull name) then
            for candidateName in [ name + ".Interactive"; name + ".DotNetInteractive" ] do
                try
                    let candidateAsm = Assembly.Load(candidateName)
                    for t in candidateAsm.GetExportedTypes() do
                        let iface = t.GetInterface(extensionInterfaceName)
                        if not (isNull iface) then
                            let onLoadAsync = t.GetMethod(onLoadAsyncName, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                            if not (isNull onLoadAsync) then
                                let ext = Activator.CreateInstance(t)
                                let task = onLoadAsync.Invoke(ext, [| null |]) :?> Task
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
