namespace Lfsi.Tests

open System
open System.IO
open System.IO.Compression
open Avalonia.Controls
open Expecto
open Lfsi.App
open Lfsi.Core

module OutputRenderingTests =
    let private bgra b g r a = [| b; g; r; a |] |> Array.map byte

    let private int32BigEndian (bytes: byte[]) offset =
        (int bytes[offset] <<< 24)
        ||| (int bytes[offset + 1] <<< 16)
        ||| (int bytes[offset + 2] <<< 8)
        ||| int bytes[offset + 3]

    let private paeth left up upperLeft =
        let p = left + up - upperLeft
        let pa = abs (p - left)
        let pb = abs (p - up)
        let pc = abs (p - upperLeft)

        if pa <= pb && pa <= pc then left
        elif pb <= pc then up
        else upperLeft

    let private pngPixels (bytes: byte[]) =
        let mutable offset = 8
        let mutable width = 0
        let mutable height = 0
        let mutable bitDepth = 0
        let mutable colorType = 0
        use idat = new MemoryStream()

        while offset < bytes.Length do
            let length = int32BigEndian bytes offset
            let chunkType = Text.Encoding.ASCII.GetString(bytes, offset + 4, 4)
            let chunkOffset = offset + 8

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

        if bitDepth <> 8 || (colorType <> 2 && colorType <> 6) then
            failwithf "Unsupported PNG format: bit depth %d, color type %d" bitDepth colorType

        let channels = if colorType = 6 then 4 else 3
        let stride = width * channels
        let compressed = idat.ToArray()
        use compressedStream = new MemoryStream(compressed, 2, compressed.Length - 6)
        use deflate = new DeflateStream(compressedStream, CompressionMode.Decompress)
        use decompressed = new MemoryStream()
        deflate.CopyTo(decompressed)

        let filtered = decompressed.ToArray()
        let pixels = Array.zeroCreate<byte> (stride * height)
        let mutable sourceOffset = 0

        for y in 0 .. height - 1 do
            let filter = int filtered[sourceOffset]
            sourceOffset <- sourceOffset + 1
            let rowOffset = y * stride
            let previousRowOffset = rowOffset - stride

            for x in 0 .. stride - 1 do
                let raw = int filtered[sourceOffset + x]

                let left =
                    if x >= channels then int pixels[rowOffset + x - channels] else 0

                let up =
                    if y > 0 then int pixels[previousRowOffset + x] else 0

                let upperLeft =
                    if y > 0 && x >= channels then int pixels[previousRowOffset + x - channels] else 0

                let value =
                    match filter with
                    | 0 -> raw
                    | 1 -> raw + left
                    | 2 -> raw + up
                    | 3 -> raw + ((left + up) / 2)
                    | 4 -> raw + paeth left up upperLeft
                    | _ -> failwithf "Unsupported PNG filter: %d" filter

                pixels[rowOffset + x] <- byte (value &&& 0xFF)

            sourceOffset <- sourceOffset + stride

        width, height, channels, pixels

    let private hasNonBackgroundPixel channels (pixels: byte[]) =
        pixels
        |> Array.chunkBySize channels
        |> Array.exists (fun px ->
            px.Length >= 3
            && (px[0] <> 24uy || px[1] <> 24uy || px[2] <> 24uy)
            && (channels = 3 || px[3] <> 0uy))

    let private pngChunk (name: string) (data: byte[]) =
        [| let length = data.Length
           yield byte ((length >>> 24) &&& 0xFF)
           yield byte ((length >>> 16) &&& 0xFF)
           yield byte ((length >>> 8) &&& 0xFF)
           yield byte (length &&& 0xFF)
           yield! Text.Encoding.ASCII.GetBytes name
           yield! data
           yield 0uy
           yield 0uy
           yield 0uy
           yield 0uy |]

    let private tinyRgbPng () =
        let width = 2
        let height = 2

        let ihdr =
            [| yield byte 0
               yield byte 0
               yield byte 0
               yield byte width
               yield byte 0
               yield byte 0
               yield byte 0
               yield byte height
               yield 8uy
               yield 2uy
               yield 0uy
               yield 0uy
               yield 0uy |]

        let raw =
            [| yield 0uy
               yield 255uy
               yield 0uy
               yield 0uy
               yield 0uy
               yield 255uy
               yield 0uy
               yield 0uy
               yield 0uy
               yield 0uy
               yield 0uy
               yield 255uy
               yield 255uy
               yield 255uy |]

        use compressed = new MemoryStream()

        do
            use zlib = new ZLibStream(compressed, CompressionMode.Compress, true)
            zlib.Write raw

        [| yield! [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]
           yield! pngChunk "IHDR" ihdr
           yield! pngChunk "IDAT" (compressed.ToArray())
           yield! pngChunk "IEND" Array.empty |]

    [<Tests>]
    let tests =
        testList
            "OutputRendering"
            [ testCase "sixel encoder emits a visible local-palette image"
              <| fun _ ->
                  let backend = SixelImageBackend()

                  let pixels =
                      [ for _ in 1..6 do
                            yield! bgra 0 0 255 255
                            yield! bgra 0 255 0 255 ]
                      |> Array.ofList

                  let sixel = backend.DiagnosticSixelSequence(2, 6, pixels)

                  Expect.stringStarts sixel "\u001bPq" "starts sixel DCS"
                  Expect.stringEnds sixel "\u001b\\" "ends sixel DCS"
                  Expect.stringContains sixel ";2;100;0;0" "defines red in the local palette"
                  Expect.stringContains sixel ";2;0;100;0" "defines green in the local palette"
                  Expect.stringContains sixel "~" "emits full sixel columns"

              testCase "sixel backend reserves chart-height output for tiny renders"
              <| fun _ ->
                  let backend = SixelImageBackend()
                  Expect.equal (backend.DiagnosticReservedRows 1) 18 "minimum reserved rows"

              testCase "sixel backend renders app output through raw terminal placement"
              <| fun _ ->
                  let backend = SixelImageBackend() :> ITerminalImageBackend

                  match backend.RenderImage { MimeType = MimeTypes.Png; Bytes = tinyRgbPng () } with
                  | Some control ->
                      Expect.isGreaterThanOrEqual control.Height 1.0 "image control reserves terminal rows"
                      Expect.isFalse (control :? TextBlock) "image rendering is emitted as raw terminal graphics"
                  | None -> failtest "Expected PNG image to render."

              testCase "chrome renderer captures a nonblank generic SVG element"
              <| fun _ ->
                  match ChromeDiscovery.resolveChromePath (ChromeDiscovery.defaultChromePath ()) with
                  | None -> printfn "Skipping Chrome render check because Chrome was not found."
                  | Some chrome ->
                      let renderer = ChromeCdpVisualOutputService(chromePath = chrome)

                      let html =
                          """<div style="width:640px;height:360px;background:#181818">
                               <svg width="640" height="360" xmlns="http://www.w3.org/2000/svg">
                                 <rect x="0" y="0" width="640" height="360" fill="#181818"/>
                                 <rect x="80" y="80" width="480" height="180" fill="#ff0000"/>
                               </svg>
                             </div>"""

                      match (renderer :> IVisualOutputService).RenderHtml html with
                      | HtmlUnsupported reason -> failtestf "Chrome render failed: %s" reason
                      | HtmlFrame frame ->
                          let width, height, channels, pixels = pngPixels frame.Bytes
                          Expect.isGreaterThanOrEqual width 640 "screenshot width"
                          Expect.isGreaterThanOrEqual height 360 "screenshot height"
                          Expect.isTrue
                              (hasNonBackgroundPixel channels pixels)
                              "screenshot has visible non-background pixels"

                          let backend = SixelImageBackend()

                          match backend.DiagnosticSixelSequenceFromPng frame.Bytes with
                          | None -> failtest "Sixel backend could not decode Chrome PNG output."
                          | Some sixel ->
                              Expect.stringContains
                                  sixel
                                  ";2;100;0;0"
                                  "generic SVG Chrome PNG remains visibly red after Sixel backend decoding" ]
