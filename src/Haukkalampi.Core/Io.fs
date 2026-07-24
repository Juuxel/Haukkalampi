// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Core.Io

open System.Buffers.Binary
open System.IO

open Haukkalampi.Core.Result

type IoError =
    | OfException of ex: System.Exception
    | WithMessage of message: string

type Result<'T> = Result<'T, IoError>

module Mutf =
    let private encodeChar (stream: Stream) (c: char) =
        if c >= '\u0001' && c <= '\u007f' then
            stream.WriteByte(byte c)
        elif c = '\u0000' || c >= '\u0080' && c <= '\u07ff' then
            let c = int c
            byte(0xC0 ||| (0x0F &&& (c >>> 12))) |> stream.WriteByte
            byte(0x80 ||| (0x3F &&& c)) |> stream.WriteByte
        else
            let c = int c
            byte(0xe0 ||| (0x0f &&& (c >>> 12))) |> stream.WriteByte
            byte(0x80 ||| (0x3f &&& (c >>> 6))) |> stream.WriteByte
            byte(0x80 ||| (0x3f &&& c)) |> stream.WriteByte

    let encode (str: string): byte array =
        use stream = new MemoryStream()
        for c in str do
            encodeChar stream c
        stream.ToArray()

    let decode (bytes: byte array): Result<string> =
        result {
            let sb = System.Text.StringBuilder()
            let mutable i = 0
            while i < Array.length bytes do
                let a = bytes[i]
                if a >>> 7 = 0uy then // 0xxx xxxx
                    let _ = sb.Append(char a)
                    i <- i + 1
                elif a >>> 5 = 0b110uy then // 110x xxxx
                    let! b =
                        if i + 1 < Array.length bytes then
                            Ok bytes[i + 1]
                        else
                            Error(WithMessage "EOF when reading two-byte MUTF-8 character")
                    let _ = sb.Append(char(((int a &&& 0x1F) <<< 6) ||| (int b &&& 0x3F)))
                    i <- i + 2
                elif a >>> 4 = 0b1110uy then // 1110 xxxx
                    let! b, c =
                        if i + 2 < Array.length bytes then
                            Ok(bytes[i + 1], bytes[i + 2])
                        else
                            Error(WithMessage "EOF when reading three-byte MUTF-8 character")
                    let _ = sb.Append(char(((int a &&& 0x0F) <<< 12) ||| ((int b &&& 0x3F) <<< 6) ||| (int c &&& 0x3F)))
                    i <- i + 3
            return sb.ToString()
        }

type DataReader =
    abstract member ReadU8: unit -> Result<byte>
    abstract member ReadI8: unit -> Result<sbyte>
    abstract member ReadU16: unit -> Result<uint16>
    abstract member ReadI16: unit -> Result<int16>
    abstract member ReadI32: unit -> Result<int32>
    abstract member ReadI64: unit -> Result<int64>
    abstract member ReadF32: unit -> Result<float32>
    abstract member ReadF64: unit -> Result<float>
    abstract member ReadRawBytes: int -> Result<byte array>
    abstract member ReadPaddedByteArray: unit -> Result<byte array>
    abstract member ReadPaddedUtf8String: unit -> Result<string>
    abstract member ReadMutf8String: unit -> Result<string>

type DataWriter =
    abstract member WriteU8: byte -> Result<unit>
    abstract member WriteI8: sbyte -> Result<unit>
    abstract member WriteU16: uint16 -> Result<unit>
    abstract member WriteI16: int16 -> Result<unit>
    abstract member WriteI32: int32 -> Result<unit>
    abstract member WriteI64: int64 -> Result<unit>
    abstract member WriteF32: float32 -> Result<unit>
    abstract member WriteF64: float -> Result<unit>
    abstract member WriteRawBytes: byte array -> Result<unit>
    abstract member WritePaddedByteArray: byte array -> Result<unit>
    abstract member WritePaddedUtf8String: string -> Result<unit>
    abstract member WriteMutf8String: string -> Result<unit>

type DataReaderWriterImpl(stream: Stream) =
    let stream = stream
    let readBytes length: Result<byte array> =
        try
            let array = Array.zeroCreate length
            stream.ReadExactly(array, 0, length)
            Ok array
        with
            | ex -> Error(OfException ex)
    let writeBytes length padding data: Result<unit> =
        try
            let actualLength = Array.length data
            let paddedData =
                if actualLength < length then
                    let init i =
                        if i < actualLength then
                            data[i]
                        else
                            padding
                    Array.init length init
                else
                    data
            Ok(stream.Write(paddedData, 0, length))
        with
            | ex -> Error(OfException ex)

    interface DataReader with
        member _.ReadU8(): Result<byte> =
            try
                match stream.ReadByte() with
                | -1 -> Error(WithMessage "reached EOS")
                | x -> Ok(byte x)
            with
                | ex -> Error(OfException ex)
        member this.ReadI8(): Result<sbyte> = 
            (this :> DataReader).ReadU8() |> Result.map sbyte
        member _.ReadU16(): Result<uint16> =
            try
                let array = [|0uy; 0uy|]
                stream.ReadExactly array
                Ok(BinaryPrimitives.ReadUInt16BigEndian array)
            with
                | ex -> Error(OfException ex)
        member this.ReadI16(): Result<int16> =
            (this :> DataReader).ReadU16() |> Result.map int16
        member _.ReadI32(): Result<int32> =
            try
                let array: byte array = Array.zeroCreate 4
                stream.ReadExactly array
                Ok(BinaryPrimitives.ReadInt32BigEndian array)
            with
                | ex -> Error(OfException ex)
        member _.ReadI64(): Result<int64> =
            try
                let array: byte array = Array.zeroCreate 8
                stream.ReadExactly array
                Ok(BinaryPrimitives.ReadInt64BigEndian array)
            with
                | ex -> Error(OfException ex)
        member _.ReadF32(): Result<float32> =
            try
                let array: byte array = Array.zeroCreate 4
                stream.ReadExactly array
                Ok(BinaryPrimitives.ReadSingleBigEndian array)
            with
                | ex -> Error(OfException ex)
        member _.ReadF64(): Result<float> =
            try
                let array: byte array = Array.zeroCreate 8
                stream.ReadExactly array
                Ok(BinaryPrimitives.ReadDoubleBigEndian array)
            with
                | ex -> Error(OfException ex)
        member _.ReadRawBytes length: Result<byte array> =
            readBytes length
        member _.ReadPaddedByteArray(): Result<byte array> =
            readBytes 1024
        member _.ReadPaddedUtf8String(): Result<string> = 
            result {
                let! data = readBytes 64
                let text = System.Text.Encoding.UTF8.GetString data
                return text.TrimEnd()
            }
        member this.ReadMutf8String(): Result<string> =
            result {
                let! length = (this :> DataReader).ReadU16()
                let! bytes = (this :> DataReader).ReadRawBytes(int length)
                return! Mutf.decode bytes
            }

    interface DataWriter with
        member _.WriteU8 input =
            try
                Ok(stream.WriteByte input)
            with
                | ex -> Error(OfException ex)
        member this.WriteI8 input = 
            (this :> DataWriter).WriteU8(byte input)
        member _.WriteU16 input =
            try
                let array = [|0uy; 0uy|]
                BinaryPrimitives.WriteUInt16BigEndian(array, input)
                Ok(stream.Write(array, 0, 2))
            with
                | ex -> Error(OfException ex)
        member this.WriteI16 input =
            (this :> DataWriter).WriteU16(uint16 input)
        member _.WriteI32 input =
            try
                let array: byte array = Array.zeroCreate 4
                BinaryPrimitives.WriteInt32BigEndian(array, input)
                Ok(stream.Write(array, 0, 4))
            with
                | ex -> Error(OfException ex)
        member _.WriteI64 input =
            try
                let array: byte array = Array.zeroCreate 8
                BinaryPrimitives.WriteInt64BigEndian(array, input)
                Ok(stream.Write(array, 0, 8))
            with
                | ex -> Error(OfException ex)
        member _.WriteF32 input =
            try
                let array: byte array = Array.zeroCreate 4
                BinaryPrimitives.WriteSingleBigEndian(array, input)
                Ok(stream.Write(array, 0, 4))
            with
                | ex -> Error(OfException ex)
        member _.WriteF64 input =
            try
                let array: byte array = Array.zeroCreate 8
                BinaryPrimitives.WriteDoubleBigEndian(array, input)
                Ok(stream.Write(array, 0, 8))
            with
                | ex -> Error(OfException ex)
        member _.WriteRawBytes input = 
            try
                Ok(stream.Write(input, 0, Array.length input))
            with
                | ex -> Error(OfException ex)
        member _.WritePaddedByteArray input =
            writeBytes 1024 0uy input
        member _.WritePaddedUtf8String input =
            let bytes = System.Text.Encoding.UTF8.GetBytes input
            writeBytes 64 0x20uy bytes
        member this.WriteMutf8String input =
            result {
                do! (this :> DataWriter).WriteU16(String.length input |> uint16)
                do! (this :> DataWriter).WriteRawBytes(Mutf.encode input)
            }
