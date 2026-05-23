namespace Lfsx.App

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Net.WebSockets
open System.Net
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Threading
open Avalonia.VisualTree
open Lfsx.Core

type NotebookTheme =
    { Dark: SolidColorBrush
      Panel: SolidColorBrush
      Text: SolidColorBrush
      Muted: SolidColorBrush
      Accent: SolidColorBrush }

type ImageFrame = { MimeType: string; Bytes: byte[] }

type HtmlRenderResult =
    | HtmlFrame of ImageFrame
    | HtmlUnsupported of string

type IVisualOutputService =
    abstract RenderHtml: html: string -> HtmlRenderResult

type ITerminalImageBackend =
    abstract Protocol: TerminalGraphicsProtocol
    abstract RenderImage: frame: ImageFrame -> Control option

type ITerminalImageLayer =
    abstract Clear: unit -> unit

type IVisualOutputCache =
    abstract Html: html: string -> HtmlRenderResult

type MemoryVisualOutputCache(visualOutputService: IVisualOutputService) =
    let html = Dictionary<string, HtmlRenderResult>()

    interface IVisualOutputCache with
        member _.Html(value) =
            match html.TryGetValue value with
            | true, result -> result
            | false, _ ->
                let result = visualOutputService.RenderHtml value
                html[value] <- result
                result

type FallbackVisualOutputService() =
    interface IVisualOutputService with
        member _.RenderHtml(_html) =
            HtmlUnsupported "HTML output detected; Chrome/CDP visual rendering is not enabled yet."

module ChromeDiscovery =
    let defaultChromePath () =
        if OperatingSystem.IsMacOS() then
            "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
        elif OperatingSystem.IsWindows() then
            "chrome.exe"
        else
            "google-chrome"

type ChromeCdpVisualOutputService(?chromePath: string, ?viewportWidth: int, ?viewportHeight: int) =
    let chromePath = defaultArg chromePath (ChromeDiscovery.defaultChromePath ())
    let viewportWidth = defaultArg viewportWidth 900
    let viewportHeight = defaultArg viewportHeight 2000

    let htmlDocument (html: string) =
        if html.Contains("<html", StringComparison.OrdinalIgnoreCase) then
            html
        else
            String.concat
                ""
                [ "<!doctype html><html><head><meta charset=\"utf-8\">"
                  "<style>html,body{margin:0;padding:0;background:#181818;overflow:hidden;}body{display:inline-block;}</style>"
                  "</head><body>"
                  html
                  "</body></html>" ]

    let availablePort () =
        let listener = new System.Net.Sockets.TcpListener(IPAddress.Loopback, 0)
        listener.Start()
        let port = (listener.LocalEndpoint :?> System.Net.IPEndPoint).Port
        listener.Stop()
        port

    let browserWebSocket port =
        task {
            use http = new HttpClient()
            let endpoint = sprintf "http://127.0.0.1:%d/json/new?about:blank" port
            let mutable lastError = ""
            let mutable result = None
            let mutable attempts = 80

            while result.IsNone && attempts > 0 do
                try
                    use request = new HttpRequestMessage(HttpMethod.Put, endpoint)
                    let! response = http.SendAsync(request)
                    let! text = response.Content.ReadAsStringAsync()
                    response.EnsureSuccessStatusCode() |> ignore
                    use document = JsonDocument.Parse(text)
                    result <- Some(document.RootElement.GetProperty("webSocketDebuggerUrl").GetString())
                with ex ->
                    lastError <- ex.Message
                    attempts <- attempts - 1
                    do! Task.Delay(100)

            return
                result
                |> Option.defaultWith (fun () -> failwith ("Chrome DevTools endpoint did not start. " + lastError))
        }

    let receiveMessage (socket: ClientWebSocket) (cancellationToken: CancellationToken) =
        task {
            let buffer = Array.zeroCreate<byte> 65536
            use stream = new MemoryStream()
            let mutable finished = false

            while not finished do
                let! result = socket.ReceiveAsync(ArraySegment<byte>(buffer), cancellationToken)

                if result.MessageType = WebSocketMessageType.Close then
                    failwith "Chrome DevTools websocket closed unexpectedly."

                stream.Write(buffer, 0, result.Count)
                finished <- result.EndOfMessage

            return Encoding.UTF8.GetString(stream.ToArray())
        }

    let sendText (socket: ClientWebSocket) (text: string) (cancellationToken: CancellationToken) =
        task {
            let bytes = Encoding.UTF8.GetBytes(text)

            do! socket.SendAsync(ArraySegment<byte>(bytes), WebSocketMessageType.Text, true, cancellationToken)
        }

    let sendCommand (socket: ClientWebSocket) id methodName paramsJson (cancellationToken: CancellationToken) =
        task {
            let message =
                sprintf """{"id":%d,"method":%s,"params":%s}""" id (JsonSerializer.Serialize(methodName)) paramsJson

            do! sendText socket message cancellationToken

            let mutable response = None

            while response.IsNone do
                let! text = receiveMessage socket cancellationToken
                use document = JsonDocument.Parse(text)
                let root = document.RootElement
                let mutable idProperty = Unchecked.defaultof<JsonElement>

                if root.TryGetProperty("id", &idProperty) then
                    if idProperty.GetInt32() = id then
                        response <- Some text

            return response.Value
        }

    let contentClipParams (socket: ClientWebSocket) commandId (cancellationToken: CancellationToken) =
        task {
            let expression =
                """
(() => {
  const target = document.body.firstElementChild || document.body;
  const rect = target.getBoundingClientRect();
  const width = Math.max(1, Math.ceil(rect.width || document.documentElement.scrollWidth || document.body.scrollWidth || 640));
  const height = Math.max(1, Math.ceil(rect.height || document.documentElement.scrollHeight || document.body.scrollHeight || 360));
  return JSON.stringify({
    x: Math.max(0, Math.floor(rect.left)),
    y: Math.max(0, Math.floor(rect.top)),
    width,
    height,
    scale: 1
  });
})()
"""

            let evaluateParams =
                sprintf """{"expression":%s,"returnByValue":true}""" (JsonSerializer.Serialize(expression))

            try
                let! response = sendCommand socket commandId "Runtime.evaluate" evaluateParams cancellationToken
                use document = JsonDocument.Parse(response)

                let value =
                    document.RootElement.GetProperty("result").GetProperty("result").GetProperty("value").GetString()

                return sprintf """{"format":"png","fromSurface":true,"clip":%s}""" value
            with _ ->
                return """{"format":"png","fromSurface":true}"""
        }

    let waitForVisualRender (socket: ClientWebSocket) commandId (cancellationToken: CancellationToken) =
        task {
            let expression =
                """
(() => !!document.querySelector('svg, canvas'))()
"""

            let evaluateParams =
                sprintf """{"expression":%s,"returnByValue":true}""" (JsonSerializer.Serialize(expression))

            let mutable isReady = false
            let mutable attempts = 30
            let mutable nextCommandId = commandId

            while not isReady && attempts > 0 do
                try
                    let! response = sendCommand socket nextCommandId "Runtime.evaluate" evaluateParams cancellationToken
                    use document = JsonDocument.Parse(response)

                    isReady <-
                        document.RootElement
                            .GetProperty("result")
                            .GetProperty("result")
                            .GetProperty("value")
                            .GetBoolean()
                with _ ->
                    isReady <- false

                if not isReady then
                    attempts <- attempts - 1
                    nextCommandId <- nextCommandId + 1
                    do! Task.Delay(250, cancellationToken)

            return nextCommandId
        }

    let renderHtmlAsync html =
        task {
            if
                String.IsNullOrWhiteSpace chromePath
                || not (
                    File.Exists chromePath
                    || chromePath = "chrome.exe"
                    || chromePath = "google-chrome"
                )
            then
                return HtmlUnsupported(sprintf "Chrome executable was not found at '%s'." chromePath)
            else
                let port = availablePort ()

                let userDataDir =
                    Path.Combine(Path.GetTempPath(), "lfsx-chrome-" + Guid.NewGuid().ToString("N"))

                let htmlPath =
                    Path.Combine(Path.GetTempPath(), "lfsx-output-" + Guid.NewGuid().ToString("N") + ".html")

                Directory.CreateDirectory(userDataDir) |> ignore
                File.WriteAllText(htmlPath, htmlDocument html)

                let startInfo =
                    ProcessStartInfo(
                        FileName = chromePath,
                        Arguments =
                            String.concat
                                " "
                                [ "--headless=new"
                                  "--disable-gpu"
                                  "--hide-scrollbars"
                                  "--no-first-run"
                                  "--no-default-browser-check"
                                  sprintf "--remote-debugging-port=%d" port
                                  sprintf "--user-data-dir=%s" userDataDir
                                  "about:blank" ],
                        UseShellExecute = false,
                        RedirectStandardError = true,
                        RedirectStandardOutput = true,
                        CreateNoWindow = true
                    )

                use proc = Process.Start(startInfo)

                let! renderResult =
                    task {
                        use timeout = new CancellationTokenSource(TimeSpan.FromSeconds 12.0)
                        let cancellationToken = timeout.Token

                        try
                            let! wsUrl = browserWebSocket port
                            use socket = new ClientWebSocket()
                            do! socket.ConnectAsync(Uri(wsUrl), cancellationToken)

                            let! _ = sendCommand socket 1 "Page.enable" "{}" cancellationToken
                            let! _ = sendCommand socket 2 "Runtime.enable" "{}" cancellationToken

                            let viewportParams =
                                sprintf
                                    """{"width":%d,"height":%d,"deviceScaleFactor":1,"mobile":false}"""
                                    viewportWidth
                                    viewportHeight

                            let! _ =
                                sendCommand
                                    socket
                                    3
                                    "Emulation.setDeviceMetricsOverride"
                                    viewportParams
                                    cancellationToken

                            let fileUrl = Uri(htmlPath).AbsoluteUri
                            let navigateParams = sprintf """{"url":%s}""" (JsonSerializer.Serialize(fileUrl))
                            let! _ = sendCommand socket 4 "Page.navigate" navigateParams cancellationToken

                            let! nextCommandId = waitForVisualRender socket 5 cancellationToken
                            let! screenshotParams = contentClipParams socket (nextCommandId + 1) cancellationToken

                            let! screenshotResponse =
                                sendCommand
                                    socket
                                    (nextCommandId + 2)
                                    "Page.captureScreenshot"
                                    screenshotParams
                                    cancellationToken

                            use document = JsonDocument.Parse(screenshotResponse)
                            let root = document.RootElement
                            let mutable errorProperty = Unchecked.defaultof<JsonElement>

                            if root.TryGetProperty("error", &errorProperty) then
                                return
                                    HtmlUnsupported(
                                        "Chrome/CDP rendering failed: "
                                        + errorProperty.GetProperty("message").GetString()
                                    )
                            else
                                let data = root.GetProperty("result").GetProperty("data").GetString()
                                let bytes = Convert.FromBase64String(data)

                                return
                                    HtmlFrame
                                        { MimeType = MimeTypes.Png
                                          Bytes = bytes }
                        with ex ->
                            return HtmlUnsupported("Chrome/CDP rendering failed: " + ex.Message)
                    }

                try
                    if not proc.HasExited then
                        proc.Kill true

                    File.Delete htmlPath
                    Directory.Delete(userDataDir, true)
                with _ ->
                    ()

                return renderResult
        }

    interface IVisualOutputService with
        member _.RenderHtml(html) =
            Task.Factory
                .StartNew(
                    Func<Task<HtmlRenderResult>>(fun () -> renderHtmlAsync html),
                    CancellationToken.None,
                    TaskCreationOptions.None,
                    TaskScheduler.Default
                )
                .Unwrap()
                .GetAwaiter()
                .GetResult()

type FallbackTerminalImageBackend(protocol: TerminalGraphicsProtocol, ?reason: string) =
    let reason = defaultArg reason "Terminal graphics backend is unavailable."

    interface ITerminalImageBackend with
        member _.Protocol = protocol
        member _.RenderImage(_frame) = None

    interface ITerminalImageLayer with
        member _.Clear() = ()

    member _.Reason = reason

type private RawTerminalImageControl
    (
        uploadSequence: string,
        imageHeight: int,
        reservedRows: int,
        emitAt: int -> int -> int -> int -> string option,
        clear: unit -> string option
    ) as this =
    inherit Control(MinHeight = float reservedRows, Height = float reservedRows)

    let mutable isEmitPending = false

    let estimatedPixelsPerRow =
        Math.Max(1.0, float imageHeight / float (Math.Max(1, reservedRows)))

    let nearestScrollViewer () =
        this.GetVisualAncestors()
        |> Seq.tryPick (function
            | :? ScrollViewer as scrollViewer -> Some scrollViewer
            | _ -> None)

    let visiblePlacement (topLevel: TopLevel) (point: Point) =
        match nearestScrollViewer () with
        | None -> Some(0, imageHeight, int (Math.Round point.Y) + 1)
        | Some scrollViewer ->
            match scrollViewer.TranslatePoint(Point(0.0, 0.0), topLevel) with
            | scrollPoint when scrollPoint.HasValue ->
                let viewportTop = scrollPoint.Value.Y
                let viewportBottom = viewportTop + scrollViewer.Bounds.Height
                let controlBottom = point.Y + this.Bounds.Height
                let firstVisibleRow = Math.Ceiling(Math.Max(point.Y, viewportTop))
                let lastVisibleRowExclusive = Math.Floor(Math.Min(controlBottom, viewportBottom))
                let visibleRows = int (lastVisibleRowExclusive - firstVisibleRow)

                if visibleRows <= 0 then
                    None
                else
                    let hiddenRows = Math.Max(0.0, firstVisibleRow - point.Y)
                    let sourceY = int (Math.Round(hiddenRows * estimatedPixelsPerRow))
                    let sourceHeight = int (Math.Round(float visibleRows * estimatedPixelsPerRow))
                    let clampedSourceY = Math.Clamp(sourceY, 0, imageHeight - 1)
                    let clampedSourceHeight = Math.Clamp(sourceHeight, 1, imageHeight - clampedSourceY)
                    Some(clampedSourceY, clampedSourceHeight, int firstVisibleRow + 1)
            | _ -> Some(0, imageHeight, int (Math.Round point.Y) + 1)

    let emit () =
        let topLevel = TopLevel.GetTopLevel(this)
        let maybePoint = this.TranslatePoint(Point(0.0, 0.0), topLevel)

        if not (isNull topLevel) && maybePoint.HasValue then
            let point = maybePoint.Value
            let column = Math.Max(1, int (Math.Round point.X) + 1)

            match visiblePlacement topLevel point with
            | Some(sourceY, sourceHeight, row) ->
                let row = Math.Max(1, row)

                match emitAt row column sourceY sourceHeight with
                | Some placement ->
                    Console.Write(sprintf "\u001b7%s\u001b[%d;%dH%s\u001b8" uploadSequence row column placement)
                    Console.Out.Flush()
                | None -> ()
            | None ->
                match clear () with
                | Some command ->
                    Console.Write(sprintf "\u001b7%s\u001b8" command)
                    Console.Out.Flush()
                | None -> ()

    member _.ScheduleEmit() =
        if not isEmitPending then
            isEmitPending <- true

            Dispatcher.UIThread.Post(
                (fun () ->
                    isEmitPending <- false
                    emit ()),
                DispatcherPriority.Background
            )

    override _.Render(context) =
        base.Render(context)
        this.ScheduleEmit()

    override _.OnDetachedFromVisualTree(args) =
        match clear () with
        | Some command ->
            Console.Write(sprintf "\u001b7%s\u001b8" command)
            Console.Out.Flush()
        | None -> ()

        base.OnDetachedFromVisualTree(args)

type KittyImageBackend(?maxChunkLength: int, ?reservedRows: int) =
    let maxChunkLength = defaultArg maxChunkLength 4096
    let fallbackReservedRows = defaultArg reservedRows 18
    let estimatedCellPixelHeight = 32.0
    let escape = "\u001b"
    let stringTerminator = escape + "\\"
    let imageIds = Dictionary<string, int>()
    let uploadedImages = HashSet<int>()
    let activePlacements = Dictionary<int, int * int * int * int * int>()
    let mutable nextImageId = 1
    let mutable nextPlacementId = 1

    let imageId imageKey =
        match imageIds.TryGetValue imageKey with
        | true, value -> value
        | false, _ ->
            let value = nextImageId
            nextImageId <- nextImageId + 1
            imageIds[imageKey] <- value
            value

    let placementId () =
        let value = nextPlacementId
        nextPlacementId <- nextPlacementId + 1
        value

    let deleteImageSequence id =
        sprintf "%s_Ga=d,d=i,i=%d;%s" escape id stringTerminator

    let deletePlacementSequence imageId placementId =
        sprintf "%s_Ga=d,d=i,i=%d,p=%d;%s" escape imageId placementId stringTerminator

    let transmissionPrefix imageId first moreChunks =
        let more = if moreChunks then 1 else 0

        if first then
            sprintf "%s_Ga=t,f=100,i=%d,q=2,m=%d;" escape imageId more
        else
            sprintf "%s_Gm=%d;" escape more

    let chunkBase64 (text: string) =
        if String.IsNullOrEmpty text then
            [ "" ]
        else
            [ for start in 0..maxChunkLength .. text.Length - 1 do
                  let length = Math.Min(maxChunkLength, text.Length - start)
                  text.Substring(start, length) ]

    let kittyImageSequence imageId bytes =
        let chunks = bytes |> Convert.ToBase64String |> chunkBase64

        chunks
        |> List.mapi (fun index chunk ->
            let isFirst = index = 0
            let hasMore = index < chunks.Length - 1
            transmissionPrefix imageId isFirst hasMore + chunk + stringTerminator)
        |> String.concat ""

    let int32BigEndian (bytes: byte[]) offset =
        (int bytes[offset] <<< 24)
        ||| (int bytes[offset + 1] <<< 16)
        ||| (int bytes[offset + 2] <<< 8)
        ||| int bytes[offset + 3]

    let isPng (bytes: byte[]) =
        bytes.Length >= 24
        && bytes[0] = 0x89uy
        && bytes[1] = 0x50uy
        && bytes[2] = 0x4Euy
        && bytes[3] = 0x47uy

    let pngDimensions (bytes: byte[]) =
        if isPng bytes then
            Some(int32BigEndian bytes 16, int32BigEndian bytes 20)
        else
            None

    let reservedRowsFor (bytes: byte[]) =
        match pngDimensions bytes with
        | Some(_, height) -> Math.Max(1, int (Math.Ceiling(float height / estimatedCellPixelHeight)))
        | None -> fallbackReservedRows

    let uploadSequence imageId bytes =
        if uploadedImages.Add imageId then
            kittyImageSequence imageId bytes
        else
            String.Empty

    let placementSequence imageId placementId sourceY sourceHeight =
        sprintf
            "%s_Ga=p,i=%d,p=%d,q=2,x=0,y=%d,h=%d,C=1;%s"
            escape
            imageId
            placementId
            sourceY
            sourceHeight
            stringTerminator

    let emitAt imageId placementId row column sourceY sourceHeight =
        match activePlacements.TryGetValue placementId with
        | true, (_, activeRow, activeColumn, activeSourceY, activeSourceHeight) when
            activeRow = row
            && activeColumn = column
            && activeSourceY = sourceY
            && activeSourceHeight = sourceHeight
            ->
            None
        | _ ->
            activePlacements[placementId] <- (imageId, row, column, sourceY, sourceHeight)
            Some(placementSequence imageId placementId sourceY sourceHeight)

    let clear imageId placementId =
        if activePlacements.Remove placementId then
            Some(deletePlacementSequence imageId placementId)
        else
            None

    let clearAll () =
        if activePlacements.Count > 0 then
            let commands =
                activePlacements.Values
                |> Seq.map (fun (imageId, _, _, _, _) -> imageId)
                |> Seq.distinct
                |> Seq.map deleteImageSequence
                |> String.concat ""

            activePlacements.Clear()
            uploadedImages.Clear()

            if not (String.IsNullOrEmpty commands) then
                Console.Write(sprintf "\u001b7%s\u001b8" commands)
                Console.Out.Flush()

    interface ITerminalImageBackend with
        member _.Protocol = Kitty

        member _.RenderImage(frame) =
            if frame.MimeType <> MimeTypes.Png then
                None
            else
                let imageKey =
                    string frame.Bytes.Length
                    + ":"
                    + Convert.ToBase64String(frame.Bytes.AsSpan(0, Math.Min(frame.Bytes.Length, 64)))

                let imageId = imageId imageKey
                let placementId = placementId ()

                let imageHeight =
                    pngDimensions frame.Bytes
                    |> Option.map snd
                    |> Option.defaultValue (fallbackReservedRows * int estimatedCellPixelHeight)

                RawTerminalImageControl(
                    uploadSequence imageId frame.Bytes,
                    imageHeight,
                    reservedRowsFor frame.Bytes,
                    emitAt imageId placementId,
                    fun () -> clear imageId placementId
                )
                :> Control
                |> Some

    interface ITerminalImageLayer with
        member _.Clear() = clearAll ()

type AvaloniaImageBackend(protocol: TerminalGraphicsProtocol) =
    interface ITerminalImageBackend with
        member _.Protocol = protocol

        member _.RenderImage(frame) =
            if frame.MimeType <> MimeTypes.Png then
                None
            else
                try
                    let stream = new MemoryStream(frame.Bytes)
                    let bitmap = new Bitmap(stream)

                    Image(
                        Source = bitmap,
                        Stretch = Stretch.Uniform,
                        HorizontalAlignment = HorizontalAlignment.Left,
                        MaxHeight = 400.0
                    )
                    :> Control
                    |> Some
                with _ ->
                    None

module OutputRendering =
    let private textBlock foreground text : Control =
        TextBlock(Text = text, Foreground = foreground, TextWrapping = TextWrapping.Wrap) :> Control

    let private binaryDescription mimeType (bytes: byte[]) =
        sprintf "[%s output: %d bytes]" mimeType bytes.Length

    let private imageFallback (frame: ImageFrame) =
        sprintf "%s Terminal graphics backend is unavailable." (binaryDescription frame.MimeType frame.Bytes)

    let private renderImage (theme: NotebookTheme) (imageBackend: ITerminalImageBackend) frame =
        match imageBackend.RenderImage frame with
        | Some control -> control
        | None -> textBlock theme.Muted (imageFallback frame)

    let renderOutput (theme: NotebookTheme) errorBrush (visualOutputCache: IVisualOutputCache) imageBackend output =
        match output with
        | NotebookOutput.Error value -> textBlock errorBrush ("[error]\n" + value)
        | NotebookOutput.Display display ->
            match display.MimeType, display.Payload with
            | MimeTypes.Text, TextPayload value -> textBlock theme.Muted value
            | MimeTypes.Html, TextPayload html ->
                match visualOutputCache.Html html with
                | HtmlFrame frame -> renderImage theme imageBackend frame
                | HtmlUnsupported reason -> textBlock theme.Muted ("[text/html]\n" + reason)
            | MimeTypes.Png, BinaryPayload bytes ->
                renderImage
                    theme
                    imageBackend
                    { MimeType = MimeTypes.Png
                      Bytes = bytes }
            | MimeTypes.Svg, TextPayload svg -> textBlock theme.Muted ("[image/svg+xml]\n" + svg)
            | mimeType, TextPayload value -> textBlock theme.Muted (sprintf "[%s]\n%s" mimeType value)
            | mimeType, BinaryPayload bytes -> textBlock theme.Muted (binaryDescription mimeType bytes)

    let renderOutputs theme errorBrush visualOutputCache imageBackend outputs =
        let panel = StackPanel(Orientation = Orientation.Vertical, Spacing = 1.0)

        outputs
        |> List.iter (fun output ->
            panel.Children.Add(renderOutput theme errorBrush visualOutputCache imageBackend output)
            |> ignore)

        panel
