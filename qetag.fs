(*
    Compile:
        fsc qetag.fs -o qetag.exe

    USAGE:
        qetag.exe <filename>
*)

open System
open System.Text
open System.IO
open System.Threading
open System.Security.Cryptography

module Util =
    type Slice<'a> = {
        buf : 'a[]
        offset : Int32
        count : Int32
    }

    let stringToUtf8 (s : String) =
        Encoding.UTF8.GetBytes(s)

    let utf8ToString (bs : byte[]) =
        Encoding.UTF8.GetString(bs)

    let concatBytes (bss : byte[][]) =
        let c = bss |> Array.map Array.length |> Array.sum
        let s = new MemoryStream(c)
        for bs in bss do
            s.Write(bs, 0, bs.Length)
        s.ToArray()

    let readerAt (input : Stream) =
        let inputLock = new Object()
        fun (offset : Int64) (length : Int32) ->
            let buf : byte[] = Array.zeroCreate length
            lock inputLock (fun _ ->
                input.Position <- offset
                let count = input.Read(buf, 0, length)
                { buf = buf; offset = 0; count = count }
            )

    let limitedParallel (limit : Int32) (jobs : Async<'a>[]) =
        async {
            let length = jobs.Length
            let count = ref -1
            let rets : 'a[] = Array.zeroCreate length
            let rec worker wid =
                async {
                    let index = Interlocked.Increment count
                    if index < length then
                        let! ret = jobs.[index]
                        rets.[index] <- ret
                        do! worker wid
                }
            do! Array.init limit worker 
                |> Async.Parallel 
                |> Async.Ignore
            return rets
        }

open Util

module Base64Safe =
    let private toUrl(c : Char) =
        match c with
        | '+' -> '-'
        | '/' -> '_'
        | _ -> c

    let private fromUrl(c : Char) =
        match c with
        | '-' -> '+'
        | '_' -> '/'
        | _ -> c

    let fromBytes (bs : byte[]) =
        Convert.ToBase64String(bs) |> String.map toUrl

    let toBytes (s : String) =
        String.map fromUrl s |> Convert.FromBase64String

    let fromString =
        stringToUtf8 >> fromBytes

    let toString =
        toBytes >> utf8ToString


module QETag =
    let sha1Slice (s : Slice<byte>) =
        use h = SHA1.Create()
        h.ComputeHash(s.buf, s.offset, s.count)

    let sha1Bytes (bs : byte[]) =
        use h = SHA1.Create()
        h.ComputeHash(bs)

    let sha1Stream (input : Stream) =
        use h = SHA1.Create()
        h.ComputeHash input

    let private hashSmall (input : Stream) = 
        let bs = sha1Stream input
        [| [| 0x16uy |]; bs |] 
        |> concatBytes 
        |> Base64Safe.fromBytes

    let private hashBig (input : Stream) =
        let worker = Environment.ProcessorCount
        let blockSize = 1 <<< 22 // 4M
        let blockCount = int32 (((input.Length + int64 blockSize - 1L) / int64 blockSize))
        let readAt = readerAt input
        let work (blockId : Int32) =
            async {
                let blockStart = int64 blockId * int64 blockSize
                return readAt blockStart blockSize |> sha1Slice
            }
        async {
            let! rets = [| 0 .. blockCount - 1 |]
                        |> Array.map work
                        |> limitedParallel worker
            let bs = rets |> concatBytes |> sha1Bytes
            return [| [| 0x96uy |]; bs |] 
                   |> concatBytes 
                   |> Base64Safe.fromBytes
        }

    let hashThreshold = 1L <<< 22 // 4M
    let hash (input : Stream) = 
        if input.Length <= hashThreshold 
        then hashSmall input 
        else hashBig input |> Async.RunSynchronously

    let hashFile (path : String) =
        use input = File.OpenRead(path)
        hash input

[<EntryPoint>]
let main _ =
    let out = Console.Out
    let args = Environment.GetCommandLineArgs()
    if args.Length < 2 then 
        out.WriteLine("The F# qetag") 
        out.WriteLine("Usage: qetag.exe <filename>")
    else out.WriteLine(QETag.hashFile args.[1])
    0
