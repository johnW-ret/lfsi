namespace Lfsi.App

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.Compression
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
open Lfsi.Core
open Microsoft.Win32

module private SixelDiag =
    let private logFile = Path.Combine(Path.GetTempPath(), "lfsx-sixel-debug.log")

    let private writer =
        lazy (new StreamWriter(logFile, true, Encoding.UTF8, AutoFlush = true))

    let log msg =
        try
            writer.Value.WriteLine(sprintf "[%s] %s" (DateTime.Now.ToString("HH:mm:ss.fff")) msg)
        with _ ->
            ()


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
    let private notEmpty value =
        value |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)

    let private existingFile path =
        path |> notEmpty |> Option.filter File.Exists

    let private registryPath key =
        try
            Registry.GetValue(key, "", null) :?> string |> existingFile
        with _ ->
            None

    let private windowsChromeCandidates () =
        let pathFromEnv name suffix =
            Environment.GetEnvironmentVariable name
            |> notEmpty
            |> Option.map (fun root -> Path.Combine(root, suffix))

        [ registryPath @"HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\App Paths\chrome.exe"
          registryPath @"HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\App Paths\chrome.exe"
          registryPath @"HKEY_LOCAL_MACHINE\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\App Paths\chrome.exe"
          pathFromEnv "ProgramFiles" @"Google\Chrome\Application\chrome.exe"
          pathFromEnv "ProgramFiles(x86)" @"Google\Chrome\Application\chrome.exe"
          pathFromEnv "LocalAppData" @"Google\Chrome\Application\chrome.exe" ]
        |> List.choose id

    let resolveChromePath path =
        match existingFile path with
        | Some path -> Some path
        | None when OperatingSystem.IsWindows() ->
            let name = Path.GetFileName(path)

            if
                name.Equals("chrome", StringComparison.OrdinalIgnoreCase)
                || name.Equals("chrome.exe", StringComparison.OrdinalIgnoreCase)
            then
                windowsChromeCandidates () |> List.tryHead
            else
                None
        | None when path = "google-chrome" || path = "chrome" || path = "chrome.exe" -> Some path
        | None -> None

    let defaultChromePath () =
        if OperatingSystem.IsMacOS() then
            "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
        elif OperatingSystem.IsWindows() then
            windowsChromeCandidates () |> List.tryHead |> Option.defaultValue "chrome.exe"
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
  const candidates = [
    ...document.querySelectorAll('svg, canvas, img, video, object, embed, iframe, body > div, body')
  ];
  const rects = candidates
    .map(element => element.getBoundingClientRect())
    .filter(rect => rect.width >= 100 && rect.height >= 100);
  const rect = rects
    .sort((a, b) => (b.width * b.height) - (a.width * a.height))[0]
    || document.body.getBoundingClientRect();
  const width = Math.min(1600, Math.max(640, Math.ceil(rect.width || document.documentElement.scrollWidth || document.body.scrollWidth || 640)));
  const height = Math.min(1200, Math.max(360, Math.ceil(rect.height || document.documentElement.scrollHeight || document.body.scrollHeight || 360)));
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
(() => {
  const candidates = document.querySelectorAll('svg, canvas, img, video, object, embed, iframe');
  return [...candidates].some(element => {
    const rect = element.getBoundingClientRect();
    return rect.width >= 100 && rect.height >= 100;
  });
})()
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
            match ChromeDiscovery.resolveChromePath chromePath with
            | None -> return HtmlUnsupported(sprintf "Chrome executable was not found at '%s'." chromePath)
            | Some resolvedChromePath ->
                let port = availablePort ()

                let userDataDir =
                    Path.Combine(Path.GetTempPath(), "lfsi-chrome-" + Guid.NewGuid().ToString("N"))

                let htmlPath =
                    Path.Combine(Path.GetTempPath(), "lfsi-output-" + Guid.NewGuid().ToString("N") + ".html")

                Directory.CreateDirectory(userDataDir) |> ignore
                File.WriteAllText(htmlPath, htmlDocument html)

                let! renderResult =
                    task {
                        try
                            let startInfo =
                                ProcessStartInfo(
                                    FileName = resolvedChromePath,
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

                            let mutable proc = Unchecked.defaultof<Process>

                            try
                                proc <- Process.Start(startInfo)
                                use timeout = new CancellationTokenSource(TimeSpan.FromSeconds 12.0)
                                let cancellationToken = timeout.Token

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
                            finally
                                try
                                    if not (isNull proc) && not proc.HasExited then
                                        proc.Kill true
                                with _ ->
                                    ()

                                if not (isNull proc) then
                                    proc.Dispose()
                        with ex ->
                            return HtmlUnsupported("Chrome/CDP rendering failed: " + ex.Message)
                    }

                try
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

module private TerminalImagePlacement =
    let estimatedPixelsPerRow imageHeight reservedRows =
        Math.Max(1.0, float imageHeight / float (Math.Max(1, reservedRows)))

    let nearestScrollViewer (control: Control) =
        control.GetVisualAncestors()
        |> Seq.tryPick (function
            | :? ScrollViewer as scrollViewer -> Some scrollViewer
            | _ -> None)

    let visiblePlacement (control: Control) imageHeight reservedRows (topLevel: TopLevel) (point: Point) =
        match nearestScrollViewer control with
        | None -> Some(0, imageHeight, int (Math.Round point.Y) + 1)
        | Some scrollViewer ->
            match scrollViewer.TranslatePoint(Point(0.0, 0.0), topLevel) with
            | scrollPoint when scrollPoint.HasValue ->
                let viewportTop = scrollPoint.Value.Y
                let viewportBottom = viewportTop + scrollViewer.Bounds.Height
                let controlBottom = point.Y + control.Bounds.Height
                let firstVisibleRow = Math.Ceiling(Math.Max(point.Y, viewportTop))
                let lastVisibleRowExclusive = Math.Floor(Math.Min(controlBottom, viewportBottom))
                let visibleRows = int (lastVisibleRowExclusive - firstVisibleRow)

                if visibleRows <= 0 then
                    None
                else
                    let hiddenRows = Math.Max(0.0, firstVisibleRow - point.Y)
                    let estimatedPixelsPerRow = estimatedPixelsPerRow imageHeight reservedRows
                    let sourceY = int (Math.Round(hiddenRows * estimatedPixelsPerRow))
                    let sourceHeight = int (Math.Round(float visibleRows * estimatedPixelsPerRow))
                    let clampedSourceY = Math.Clamp(sourceY, 0, imageHeight - 1)
                    let clampedSourceHeight = Math.Clamp(sourceHeight, 1, imageHeight - clampedSourceY)
                    Some(clampedSourceY, clampedSourceHeight, int firstVisibleRow + 1)
            | _ -> Some(0, imageHeight, int (Math.Round point.Y) + 1)

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

    let visiblePlacement (topLevel: TopLevel) (point: Point) =
        TerminalImagePlacement.visiblePlacement this imageHeight reservedRows topLevel point

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

type private RawSixelImageControl
    (imageHeight: int, reservedRows: int, rasterWidth: int, generateSixel: int -> int -> string) as this =
    inherit Control(MinHeight = float reservedRows, Height = float reservedRows)

    do
        SixelDiag.log (
            sprintf "RawSixelImageControl created: imgH=%d resRows=%d rasterW=%d" imageHeight reservedRows rasterWidth
        )

    let mutable lastEmittedRow = 0
    let mutable lastEmittedColumn = 0
    let mutable lastEmittedRows = 0
    let mutable lastEmittedSourceHeight = 0
    let mutable isAttached = false
    let mutable cachedSixelData: string option = None
    let reEmitTimer = new DispatcherTimer(Interval = TimeSpan.FromMilliseconds(150.0))

    let estimatedPixelsPerRow =
        TerminalImagePlacement.estimatedPixelsPerRow imageHeight reservedRows

    let visiblePlacement (topLevel: TopLevel) (point: Point) =
        TerminalImagePlacement.visiblePlacement this imageHeight reservedRows topLevel point

    /// Generate a blank sixel that overwrites old pixel data with empty (no-pixel) bands.
    let blankSixel width height =
        let escape = "\u001b"
        let builder = StringBuilder()
        builder.Append(escape).Append("Pq") |> ignore
        builder.Append(sprintf "\"1;1;%d;%d" width height) |> ignore
        // Define color 0 as black
        builder.Append("#0;2;0;0;0") |> ignore
        let bandCount = int (Math.Ceiling(float height / 6.0))

        for bandIndex in 0 .. bandCount - 1 do
            builder.Append("#0") |> ignore

            for _ in 0 .. width - 1 do
                builder.Append('?') |> ignore

            if bandIndex < bandCount - 1 then
                builder.Append('-') |> ignore

        builder.Append(escape).Append('\\') |> ignore
        builder.ToString()

    /// Erase old sixel pixels by overwriting with a blank sixel at the last emitted position.
    let clearOldSixel () =
        if lastEmittedRows > 0 && lastEmittedSourceHeight > 0 then
            let blankData = blankSixel rasterWidth lastEmittedSourceHeight
            Console.Write(sprintf "\u001b7\u001b[%d;%dH%s\u001b8" lastEmittedRow lastEmittedColumn blankData)
            Console.Out.Flush()
            lastEmittedRows <- 0
            lastEmittedSourceHeight <- 0
            cachedSixelData <- None

    let emit () =
        if not isAttached then
            ()
        else
            let topLevel = TopLevel.GetTopLevel(this)
            let maybePoint = this.TranslatePoint(Point(0.0, 0.0), topLevel)

            if not (isNull topLevel) && maybePoint.HasValue then
                let point = maybePoint.Value
                let column = Math.Max(1, int (Math.Round point.X) + 1)

                match visiblePlacement topLevel point with
                | Some(sourceY, sourceHeight, row) ->
                    let row = Math.Max(1, row)
                    let visibleRows = int (Math.Ceiling(float sourceHeight / estimatedPixelsPerRow))
                    // If position changed, erase old sixel pixels first
                    if lastEmittedRows > 0 && (lastEmittedRow <> row || lastEmittedColumn <> column) then
                        clearOldSixel ()

                    let sixelData = generateSixel sourceY sourceHeight
                    Console.Write(sprintf "\u001b7\u001b[%d;%dH%s\u001b8" row column sixelData)
                    Console.Out.Flush()
                    cachedSixelData <- Some sixelData
                    lastEmittedRow <- row
                    lastEmittedColumn <- column
                    lastEmittedRows <- visibleRows + 1
                    lastEmittedSourceHeight <- sourceHeight
                | None -> clearOldSixel ()

    let reEmit () =
        if not isAttached then
            ()
        else
            let topLevel = TopLevel.GetTopLevel(this)
            let maybePoint = this.TranslatePoint(Point(0.0, 0.0), topLevel)

            if isNull topLevel || not maybePoint.HasValue then
                ()
            else
                let point = maybePoint.Value
                let column = Math.Max(1, int (Math.Round point.X) + 1)

                match visiblePlacement topLevel point with
                | Some(_sourceY, _sourceHeight, row) ->
                    let row = Math.Max(1, row)

                    if row = lastEmittedRow && column = lastEmittedColumn then
                        match cachedSixelData with
                        | Some sixelData ->
                            Console.Write(sprintf "\u001b7\u001b[%d;%dH%s\u001b8" row column sixelData)
                            Console.Out.Flush()
                        | None -> emit ()
                    else
                        emit ()
                | None -> clearOldSixel ()

    do reEmitTimer.Tick.Add(fun _ -> reEmit ())

    override _.OnAttachedToVisualTree(args) =
        base.OnAttachedToVisualTree(args)
        isAttached <- true
        Dispatcher.UIThread.Post((fun () -> emit ()), DispatcherPriority.Background)
        reEmitTimer.Start()

    override _.Render(context) =
        base.Render(context)
        Dispatcher.UIThread.Post((fun () -> emit ()), DispatcherPriority.Background)

    override _.OnDetachedFromVisualTree(args) =
        isAttached <- false
        reEmitTimer.Stop()
        clearOldSixel ()
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

type private SixelRaster =
    { Width: int
      Height: int
      Pixels: byte[] }

type SixelImageBackend(?reservedRows: int) =
    let fallbackReservedRows = defaultArg reservedRows 18
    let estimatedCellPixelHeight = 16.0
    let maxRasterWidth = 800
    let maxRasterHeight = 320
    let escape = "\u001b"
    let rasterCache = Dictionary<string, SixelRaster>()

    let colorLevel (value: byte) =
        Math.Clamp((int value * 5 + 127) / 255, 0, 5)

    let colorKey (r: byte) (g: byte) (b: byte) =
        16 + colorLevel r * 36 + colorLevel g * 6 + colorLevel b

    let colorPercent level = level * 20

    let colorDefinition register index =
        let local = index - 16
        let r = local / 36
        let g = (local / 6) % 6
        let b = local % 6
        sprintf "#%d;2;%d;%d;%d" register (colorPercent r) (colorPercent g) (colorPercent b)

    let pixelColorIndex (raster: SixelRaster) x y =
        let offset = (y * raster.Width + x) * 4
        let b = raster.Pixels[offset]
        let g = raster.Pixels[offset + 1]
        let r = raster.Pixels[offset + 2]
        let a = raster.Pixels[offset + 3]

        if a = 255uy then
            colorKey r g b
        else
            let alpha = int a

            let composite channel =
                byte (((int channel * alpha) + (24 * (255 - alpha))) / 255)

            colorKey (composite r) (composite g) (composite b)

    let appendRepeated (builder: StringBuilder) (count: int) (value: char) =
        if count >= 4 then
            builder.Append('!').Append(count).Append(value) |> ignore
        else
            for _ in 1..count do
                builder.Append(value) |> ignore

    let appendColorBand (builder: StringBuilder) (raster: SixelRaster) sourceY bandY sourceHeight colorKey =
        let mutable runChar = char 0
        let mutable runLength = 0

        for x in 0 .. raster.Width - 1 do
            let mutable bits = 0

            for bit in 0..5 do
                let cropY = bandY + bit

                if cropY < sourceHeight then
                    let y = sourceY + cropY

                    if y < raster.Height && pixelColorIndex raster x y = colorKey then
                        bits <- bits ||| (1 <<< bit)

            let ch = char (63 + bits)

            if runLength = 0 then
                runChar <- ch
                runLength <- 1
            elif ch = runChar then
                runLength <- runLength + 1
            else
                appendRepeated builder runLength runChar
                runChar <- ch
                runLength <- 1

        if runLength > 0 then
            appendRepeated builder runLength runChar

    let bandColors (raster: SixelRaster) sourceY bandY sourceHeight =
        let colors = SortedSet<int>()

        for yOffset in bandY .. Math.Min(sourceHeight - 1, bandY + 5) do
            let y = sourceY + yOffset

            if y < raster.Height then
                for x in 0 .. raster.Width - 1 do
                    colors.Add(pixelColorIndex raster x y) |> ignore

        colors |> Seq.toList

    let sixelSequence raster sourceY sourceHeight =
        let sourceY = Math.Clamp(sourceY, 0, Math.Max(0, raster.Height - 1))
        let sourceHeight = Math.Clamp(sourceHeight, 1, raster.Height - sourceY)
        let builder = StringBuilder()
        let allColors = SortedSet<int>()

        for y in sourceY .. sourceY + sourceHeight - 1 do
            for x in 0 .. raster.Width - 1 do
                allColors.Add(pixelColorIndex raster x y) |> ignore

        let palette =
            allColors |> Seq.mapi (fun register colorKey -> colorKey, register) |> dict

        builder.Append(escape).Append("Pq") |> ignore
        builder.Append(sprintf "\"1;1;%d;%d" raster.Width sourceHeight) |> ignore

        for KeyValue(colorKey, register) in palette do
            builder.Append(colorDefinition register colorKey) |> ignore

        let bandCount = int (Math.Ceiling(float sourceHeight / 6.0))

        for bandIndex in 0 .. bandCount - 1 do
            let bandY = bandIndex * 6
            let colors = bandColors raster sourceY bandY sourceHeight

            colors
            |> List.iteri (fun index colorKey ->
                builder.Append('#').Append(palette[colorKey]) |> ignore
                appendColorBand builder raster sourceY bandY sourceHeight colorKey

                if index < colors.Length - 1 then
                    builder.Append('$') |> ignore)

            if bandIndex < bandCount - 1 then
                builder.Append('-') |> ignore

        builder.Append(escape).Append('\\') |> ignore
        builder.ToString()

    let autocropRaster (raster: SixelRaster) =
        // Detect the dominant background color (bottom-right pixel)
        let bgOff = ((raster.Height - 1) * raster.Width + (raster.Width - 1)) * 4

        let bgB, bgG, bgR =
            raster.Pixels.[bgOff], raster.Pixels.[bgOff + 1], raster.Pixels.[bgOff + 2]

        let threshold = 30 // color distance threshold for "same as background"

        let isBg x y =
            let off = (y * raster.Width + x) * 4

            abs (int raster.Pixels.[off] - int bgB)
            + abs (int raster.Pixels.[off + 1] - int bgG)
            + abs (int raster.Pixels.[off + 2] - int bgR) < threshold
        // Find content bounds by scanning edges
        let mutable minX = raster.Width
        let mutable minY = raster.Height
        let mutable maxX = 0
        let mutable maxY = 0
        // Sample every 4th pixel for speed on large images
        let step = Math.Max(1, Math.Min(raster.Width, raster.Height) / 500)

        for y in 0..step .. raster.Height - 1 do
            for x in 0..step .. raster.Width - 1 do
                if not (isBg x y) then
                    minX <- Math.Min(minX, x)
                    minY <- Math.Min(minY, y)
                    maxX <- Math.Max(maxX, x)
                    maxY <- Math.Max(maxY, y)

        if maxX <= minX || maxY <= minY then
            raster // no content found, return original
        else
            // Add margin
            let margin = Math.Max(step, 4)
            let cropX = Math.Max(0, minX - margin)
            let cropY = Math.Max(0, minY - margin)
            let cropW = Math.Min(raster.Width - cropX, maxX - cropX + margin + 1)
            let cropH = Math.Min(raster.Height - cropY, maxY - cropY + margin + 1)

            SixelDiag.log (
                sprintf "autocrop: %dx%d -> %dx%d (crop from %d,%d)" raster.Width raster.Height cropW cropH cropX cropY
            )

            if cropW >= raster.Width * 3 / 4 && cropH >= raster.Height * 3 / 4 then
                raster // content fills most of image, no crop needed
            else
                let pixels = Array.zeroCreate<byte> (cropW * cropH * 4)

                for y in 0 .. cropH - 1 do
                    let srcOff = ((cropY + y) * raster.Width + cropX) * 4
                    let dstOff = y * cropW * 4
                    Array.Copy(raster.Pixels, srcOff, pixels, dstOff, cropW * 4)

                { Width = cropW
                  Height = cropH
                  Pixels = pixels }

    let scaleRaster (raster: SixelRaster) =
        let scale =
            Math.Min(
                1.0,
                Math.Min(float maxRasterWidth / float raster.Width, float maxRasterHeight / float raster.Height)
            )

        if scale >= 1.0 then
            raster
        else
            let width = Math.Max(1, int (Math.Round(float raster.Width * scale)))
            let height = Math.Max(1, int (Math.Round(float raster.Height * scale)))
            let pixels = Array.zeroCreate<byte> (width * height * 4)
            let invScale = 1.0 / scale

            for y in 0 .. height - 1 do
                let srcY0 = int (Math.Floor(float y * invScale))

                let srcY1 =
                    Math.Min(raster.Height - 1, int (Math.Floor(float (y + 1) * invScale)) - 1)

                let srcY1 = Math.Max(srcY0, srcY1)

                for x in 0 .. width - 1 do
                    let srcX0 = int (Math.Floor(float x * invScale))

                    let srcX1 =
                        Math.Min(raster.Width - 1, int (Math.Floor(float (x + 1) * invScale)) - 1)

                    let srcX1 = Math.Max(srcX0, srcX1)
                    let mutable sumB = 0
                    let mutable sumG = 0
                    let mutable sumR = 0
                    let mutable sumA = 0
                    let mutable count = 0

                    for sy in srcY0..srcY1 do
                        for sx in srcX0..srcX1 do
                            let off = (sy * raster.Width + sx) * 4
                            sumB <- sumB + int raster.Pixels[off]
                            sumG <- sumG + int raster.Pixels[off + 1]
                            sumR <- sumR + int raster.Pixels[off + 2]
                            sumA <- sumA + int raster.Pixels[off + 3]
                            count <- count + 1

                    let targetOffset = (y * width + x) * 4

                    if count > 0 then
                        pixels[targetOffset] <- byte (sumB / count)
                        pixels[targetOffset + 1] <- byte (sumG / count)
                        pixels[targetOffset + 2] <- byte (sumR / count)
                        pixels[targetOffset + 3] <- byte (sumA / count)

            { Width = width
              Height = height
              Pixels = pixels }

    let int32BigEndian (bytes: byte[]) offset =
        (int bytes[offset] <<< 24)
        ||| (int bytes[offset + 1] <<< 16)
        ||| (int bytes[offset + 2] <<< 8)
        ||| int bytes[offset + 3]

    let paeth left up upperLeft =
        let p = left + up - upperLeft
        let pa = abs (p - left)
        let pb = abs (p - up)
        let pc = abs (p - upperLeft)

        if pa <= pb && pa <= pc then left
        elif pb <= pc then up
        else upperLeft

    let decodePng (bytes: byte[]) =
        try
            if
                bytes.Length < 8
                || bytes[0] <> 0x89uy
                || bytes[1] <> 0x50uy
                || bytes[2] <> 0x4Euy
                || bytes[3] <> 0x47uy
            then
                None
            else
                let mutable offset = 8
                let mutable width = 0
                let mutable height = 0
                let mutable bitDepth = 0
                let mutable colorType = 0
                use idat = new MemoryStream()

                while offset + 8 <= bytes.Length do
                    let length = int32BigEndian bytes offset
                    let chunkType = Encoding.ASCII.GetString(bytes, offset + 4, 4)
                    let chunkOffset = offset + 8

                    if chunkOffset + length + 4 > bytes.Length then
                        offset <- bytes.Length
                    else
                        match chunkType with
                        | "IHDR" ->
                            width <- int32BigEndian bytes chunkOffset
                            height <- int32BigEndian bytes (chunkOffset + 4)
                            bitDepth <- int bytes[chunkOffset + 8]
                            colorType <- int bytes[chunkOffset + 9]
                        | "IDAT" -> idat.Write(bytes, chunkOffset, length)
                        | "IEND" -> offset <- bytes.Length
                        | _ -> ()

                        if offset < bytes.Length then
                            offset <- chunkOffset + length + 4

                SixelDiag.log (
                    sprintf
                        "decodePng: IHDR w=%d h=%d bitDepth=%d colorType=%d idatLen=%d"
                        width
                        height
                        bitDepth
                        colorType
                        (int idat.Length)
                )

                if width <= 0 || height <= 0 || bitDepth <> 8 || (colorType <> 2 && colorType <> 6) then
                    SixelDiag.log (sprintf "decodePng: REJECTED format (bitDepth=%d colorType=%d)" bitDepth colorType)
                    None
                else
                    let channels = if colorType = 6 then 4 else 3
                    let sourceStride = width * channels
                    let compressed = idat.ToArray()
                    use compressedStream = new MemoryStream(compressed)
                    use zlib = new ZLibStream(compressedStream, CompressionMode.Decompress)
                    use decompressed = new MemoryStream()
                    zlib.CopyTo(decompressed)

                    let filtered = decompressed.ToArray()
                    let expectedFilteredLen = (sourceStride + 1) * height

                    SixelDiag.log (
                        sprintf
                            "decodePng: channels=%d sourceStride=%d filteredLen=%d expectedLen=%d"
                            channels
                            sourceStride
                            filtered.Length
                            expectedFilteredLen
                    )
                    // Sample the raw filtered data for the first few filter bytes
                    SixelDiag.log (
                        sprintf
                            "decodePng: first 10 filter bytes: %s"
                            (String.Join(
                                ",",
                                [| for i in 0 .. Math.Min(9, height - 1) ->
                                       string (int filtered[i * (sourceStride + 1)]) |]
                            ))
                    )

                    let sourcePixels = Array.zeroCreate<byte> (sourceStride * height)
                    let mutable sourceOffset = 0

                    for y in 0 .. height - 1 do
                        let filter = int filtered[sourceOffset]
                        sourceOffset <- sourceOffset + 1
                        let rowOffset = y * sourceStride
                        let previousRowOffset = rowOffset - sourceStride

                        for x in 0 .. sourceStride - 1 do
                            let raw = int filtered[sourceOffset + x]

                            let left =
                                if x >= channels then
                                    int sourcePixels[rowOffset + x - channels]
                                else
                                    0

                            let up = if y > 0 then int sourcePixels[previousRowOffset + x] else 0

                            let upperLeft =
                                if y > 0 && x >= channels then
                                    int sourcePixels[previousRowOffset + x - channels]
                                else
                                    0

                            let value =
                                match filter with
                                | 0 -> raw
                                | 1 -> raw + left
                                | 2 -> raw + up
                                | 3 -> raw + ((left + up) / 2)
                                | 4 -> raw + paeth left up upperLeft
                                | _ -> failwithf "Unsupported PNG filter: %d" filter

                            sourcePixels[rowOffset + x] <- byte (value &&& 0xFF)

                        sourceOffset <- sourceOffset + sourceStride

                    let pixels = Array.zeroCreate<byte> (width * height * 4)

                    for y in 0 .. height - 1 do
                        for x in 0 .. width - 1 do
                            let sourcePixelOffset = y * sourceStride + x * channels
                            let targetPixelOffset = (y * width + x) * 4
                            pixels[targetPixelOffset] <- sourcePixels[sourcePixelOffset + 2]
                            pixels[targetPixelOffset + 1] <- sourcePixels[sourcePixelOffset + 1]
                            pixels[targetPixelOffset + 2] <- sourcePixels[sourcePixelOffset]

                            pixels[targetPixelOffset + 3] <-
                                if channels = 4 then
                                    sourcePixels[sourcePixelOffset + 3]
                                else
                                    255uy

                    { Width = width
                      Height = height
                      Pixels = pixels }
                    |> autocropRaster
                    |> fun originalRaster ->
                        // Sample original before scaling
                        let sp x y =
                            if x < originalRaster.Width && y < originalRaster.Height then
                                let o = (y * originalRaster.Width + x) * 4

                                sprintf
                                    "(%d,%d,%d)"
                                    originalRaster.Pixels.[o + 2]
                                    originalRaster.Pixels.[o + 1]
                                    originalRaster.Pixels.[o]
                            else
                                "OOB"

                        SixelDiag.log (
                            sprintf
                                "decodePng: ORIGINAL samples RGB: [0,0]=%s [100,100]=%s [500,500]=%s [1000,1000]=%s [4500,4500]=%s [8999,8999]=%s"
                                (sp 0 0)
                                (sp 100 100)
                                (sp 500 500)
                                (sp 1000 1000)
                                (sp 4500 4500)
                                (sp 8999 8999)
                        )

                        scaleRaster originalRaster
                    |> Some
        with _ ->
            None

    let rasterKey (bytes: byte[]) =
        string bytes.Length
        + ":"
        + Convert.ToBase64String(bytes.AsSpan(0, Math.Min(bytes.Length, 64)))

    let decodeWithAvalonia (bytes: byte[]) =
        try
            use stream = new MemoryStream(bytes)
            let bitmap = new Bitmap(stream)
            let srcW = bitmap.PixelSize.Width
            let srcH = bitmap.PixelSize.Height
            SixelDiag.log (sprintf "decodeWithAvalonia: src=%dx%d" srcW srcH)

            let scale =
                Math.Min(1.0, Math.Min(float maxRasterWidth / float srcW, float maxRasterHeight / float srcH))

            let targetW = Math.Max(1, int (Math.Round(float srcW * scale)))
            let targetH = Math.Max(1, int (Math.Round(float srcH * scale)))
            SixelDiag.log (sprintf "decodeWithAvalonia: scale=%g target=%dx%d" scale targetW targetH)

            let rtb =
                new RenderTargetBitmap(Avalonia.PixelSize(targetW, targetH), Vector(96.0, 96.0))

            use ctx = rtb.CreateDrawingContext()
            ctx.DrawImage(bitmap, Rect(0.0, 0.0, float srcW, float srcH), Rect(0.0, 0.0, float targetW, float targetH))
            ctx.Dispose()
            // Save to PNG then decode with our simple decoder to get pixels
            // This is simpler than trying to lock the RenderTargetBitmap pixels
            use pngStream = new MemoryStream()
            rtb.Save(pngStream)
            let pngBytes = pngStream.ToArray()
            SixelDiag.log (sprintf "decodeWithAvalonia: re-encoded PNG size=%d" pngBytes.Length)
            // Decode the smaller PNG with our custom decoder (it will be at targetW x targetH)
            match decodePng pngBytes with
            | Some raster ->
                SixelDiag.log (sprintf "decodeWithAvalonia: re-decoded to %dx%d" raster.Width raster.Height)
                let pixelData = raster.Pixels
                ignore pixelData // raster is already the right format
                Some raster
            | None ->
                SixelDiag.log "decodeWithAvalonia: re-decode failed"
                None
        with ex ->
            SixelDiag.log (sprintf "decodeWithAvalonia failed: %s" ex.Message)
            None

    let rasterFor (bytes: byte[]) =
        let key = rasterKey bytes

        match rasterCache.TryGetValue key with
        | true, raster -> Some raster
        | false, _ ->
            match decodeWithAvalonia bytes with
            | Some raster ->
                rasterCache[key] <- raster
                Some raster
            | None ->
                match decodePng bytes with
                | Some raster ->
                    rasterCache[key] <- raster
                    Some raster
                | None -> None

    let reservedRowsFor height =
        Math.Max(fallbackReservedRows, int (Math.Ceiling(float height / estimatedCellPixelHeight)))

    let rawSixelControl (raster: SixelRaster) =
        RawSixelImageControl(
            raster.Height,
            reservedRowsFor raster.Height,
            raster.Width,
            (fun sourceY sourceHeight -> sixelSequence raster sourceY sourceHeight)
        )
        :> Control


    /// Exposed for smoke tests and focused backend tests; normal rendering goes through RenderImage.
    member _.DiagnosticSixelSequence(width: int, height: int, pixels: byte[], ?sourceY: int, ?sourceHeight: int) =
        let raster =
            { Width = width
              Height = height
              Pixels = pixels }

        sixelSequence raster (defaultArg sourceY 0) (defaultArg sourceHeight height)

    /// Exposed for tests that need to verify PNG decoding and sixel encoding together.
    member _.DiagnosticSixelSequenceFromPng(bytes: byte[]) =
        decodePng bytes
        |> Option.map (fun raster -> sixelSequence raster 0 raster.Height)

    /// Exposed for tests that verify layout reservation without rendering a control tree.
    member _.DiagnosticReservedRows(height: int) = reservedRowsFor height

    interface ITerminalImageBackend with
        member _.Protocol = Sixel

        member _.RenderImage(frame) =
            SixelDiag.log (sprintf "RenderImage: mime=%s len=%d" frame.MimeType frame.Bytes.Length)

            if frame.MimeType <> MimeTypes.Png then
                SixelDiag.log "RenderImage: not PNG"
                None
            else
                match rasterFor frame.Bytes with
                | None ->
                    SixelDiag.log "RenderImage: decode failed"
                    None
                | Some raster ->
                    SixelDiag.log (
                        sprintf
                            "RenderImage: raster %dx%d pixelBytes=%d"
                            raster.Width
                            raster.Height
                            raster.Pixels.Length
                    )
                    // Sample some pixels for debugging
                    let samplePixel x y =
                        if x < raster.Width && y < raster.Height then
                            let off = (y * raster.Width + x) * 4

                            sprintf
                                "(%d,%d,%d,%d)"
                                raster.Pixels.[off]
                                raster.Pixels.[off + 1]
                                raster.Pixels.[off + 2]
                                raster.Pixels.[off + 3]
                        else
                            "OOB"

                    SixelDiag.log (
                        sprintf
                            "RenderImage: pixels (BGRA): [0,0]=%s [w/2,h/2]=%s [0,h-1]=%s"
                            (samplePixel 0 0)
                            (samplePixel (raster.Width / 2) (raster.Height / 2))
                            (samplePixel 0 (raster.Height - 1))
                    )
                    // Count unique colors in 216-color cube
                    let colorSet = System.Collections.Generic.HashSet<int>()

                    for y in 0 .. raster.Height - 1 do
                        for x in 0 .. raster.Width - 1 do
                            let off = (y * raster.Width + x) * 4
                            let b = raster.Pixels.[off]
                            let g = raster.Pixels.[off + 1]
                            let r = raster.Pixels.[off + 2]
                            let a = raster.Pixels.[off + 3]
                            let rl = Math.Clamp((int r * 5 + 127) / 255, 0, 5)
                            let gl = Math.Clamp((int g * 5 + 127) / 255, 0, 5)
                            let bl = Math.Clamp((int b * 5 + 127) / 255, 0, 5)
                            colorSet.Add(16 + rl * 36 + gl * 6 + bl) |> ignore

                    SixelDiag.log (sprintf "RenderImage: unique quantized colors=%d" colorSet.Count)
                    rawSixelControl raster |> Some

    interface ITerminalImageLayer with
        member _.Clear() = rasterCache.Clear()

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
