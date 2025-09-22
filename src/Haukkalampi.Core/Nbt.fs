namespace Haukkalampi.Core.Nbt

open Haukkalampi.Core.Io
open Haukkalampi.Core.Result

type NbtElement =
    | NbtEnd
    | NbtByte of Value: sbyte
    | NbtShort of Value: int16
    | NbtInt of Value: int32
    | NbtLong of Value: int64
    | NbtFloat of Value: float32
    | NbtDouble of Value: float
    | NbtByteArray of Value: sbyte list
    | NbtString of Value: string
    | NbtList of Value: NbtElement list
    | NbtCompound of Value: Map<string, NbtElement>
    | NbtIntArray of Value: int32 list
    | NbtLongArray of Value: int64 list
    member this.NbtTag =
        match this with
        | NbtEnd -> 0uy
        | NbtByte _ -> 1uy
        | NbtShort _ -> 2uy
        | NbtInt _ -> 3uy
        | NbtLong _ -> 4uy
        | NbtFloat _ -> 5uy
        | NbtDouble _ -> 6uy
        | NbtByteArray _ -> 7uy
        | NbtString _ -> 8uy
        | NbtList _ -> 9uy
        | NbtCompound _ -> 10uy
        | NbtIntArray _ -> 11uy
        | NbtLongArray _ -> 12uy

    member this.Encode(writer: DataWriter) =
        result {
            match this with
            | NbtEnd -> ()
            | NbtByte value ->
                do! writer.WriteI8 value
            | NbtShort value ->
                do! writer.WriteI16 value
            | NbtInt value ->
                do! writer.WriteI32 value
            | NbtLong value ->
                do! writer.WriteI64 value
            | NbtFloat value ->
                do! writer.WriteF32 value
            | NbtDouble value ->
                do! writer.WriteF64 value
            | NbtByteArray value ->
                let len = List.length value
                do! writer.WriteI32 len
                let rawBytes: byte array = Array.zeroCreate len
                let rec convert l i =
                    match l with
                    | x :: xs ->
                        rawBytes[i] <- byte x
                        convert xs (i + 1)
                    | _ -> ()
                convert value 0
                do! writer.WriteRawBytes rawBytes
            | NbtString value ->
                do! writer.WriteMutf8String value
            | NbtList value ->
                match value with
                | [] ->
                    do! writer.WriteU8 NbtEnd.NbtTag
                    do! writer.WriteI32 0
                | head :: _ ->
                    let elementTag = head.NbtTag
                    do! writer.WriteU8 elementTag
                    do! List.length value |> writer.WriteI32
                    for element in value do
                        if element.NbtTag <> elementTag then
                            return! Error(WithMessage $"Cannot write non-homogeneous NBT list: {value}")
                        do! element.Encode writer
            | NbtCompound value ->
                for key, child in Map.toSeq value do
                    do! child.EncodeNamed writer key
                do! writer.WriteU8 NbtEnd.NbtTag
            | NbtIntArray value ->
                do! writer.WriteI32(List.length value)
                for x in value do
                    do! writer.WriteI32 x
            | NbtLongArray value ->
                do! writer.WriteI32(List.length value)
                for x in value do
                    do! writer.WriteI64 x
        }

    member this.EncodeNamed (writer: DataWriter) name =
        result {
            do! writer.WriteU8 this.NbtTag
            do! writer.WriteMutf8String name
            do! this.Encode writer
        }

module NbtElement =
    let rec decodeWithKnownTag (reader: DataReader) tag: Result<NbtElement> =
        match tag with
        | 0uy -> Ok NbtEnd
        | 1uy -> reader.ReadI8() |> Result.map NbtByte
        | 2uy -> reader.ReadI16() |> Result.map NbtShort
        | 3uy -> reader.ReadI32() |> Result.map NbtInt
        | 4uy -> reader.ReadI64() |> Result.map NbtLong
        | 5uy -> reader.ReadF32() |> Result.map NbtFloat
        | 6uy -> reader.ReadF64() |> Result.map NbtDouble
        | 7uy ->
            result {
                let! size = reader.ReadI32()
                let! bytes = reader.ReadRawBytes size
                return List.init size (fun i -> sbyte bytes[i]) |> NbtByteArray
            }
        | 8uy -> reader.ReadMutf8String() |> Result.map NbtString
        | 9uy ->
            result {
                let! elementTag = reader.ReadU8()
                let! size = reader.ReadI32()
                let buffer = System.Collections.Generic.List<NbtElement> size
                for _ = 1 to size do
                    let! child = decodeWithKnownTag reader elementTag
                    buffer.Add child
                return NbtList(Seq.toList buffer)
            }
        | 10uy ->
            let buffer = System.Collections.Generic.List<string * NbtElement>()
            let rec inner() =
                result {
                    let! tag = reader.ReadU8()
                    if tag <> 0uy then
                        let! entry = decodeNamedWithKnownTag reader tag
                        buffer.Add entry
                        return! inner()
                    else
                        return ()
                }
            inner() |> Result.map(fun() -> NbtCompound(Map.ofSeq buffer))
        | 11uy ->
            result {
                let! size = reader.ReadI32()
                let buffer = System.Collections.Generic.List<int> size
                for _ = 1 to size do
                    let! element = reader.ReadI32()
                    buffer.Add element
                return NbtIntArray(List.ofSeq buffer)
            }
        | 12uy ->
            result {
                let! size = reader.ReadI32()
                let buffer = System.Collections.Generic.List<int64> size
                for _ = 1 to size do
                    let! element = reader.ReadI64()
                    buffer.Add element
                return NbtLongArray(List.ofSeq buffer)
            }
        | _ -> Error(WithMessage $"Unknown NBT tag type: {tag}")
    and decodeNamedWithKnownTag (reader: DataReader) tag: Result<string * NbtElement> =
        result {
            let! name = reader.ReadMutf8String()
            let! nbt = decodeWithKnownTag reader tag
            return name, nbt
        }

    let decode(reader: DataReader): Result<NbtElement> =
        reader.ReadU8() |> Result.bind(decodeWithKnownTag reader)

    let decodeNamed(reader: DataReader): Result<string * NbtElement> =
        reader.ReadU8() |> Result.bind(decodeNamedWithKnownTag reader)
