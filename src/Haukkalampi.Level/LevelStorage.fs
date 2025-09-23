module Haukkalampi.Level.Storage

open Haukkalampi.Core.Collection
open Haukkalampi.Core.Io
open Haukkalampi.Core.Nbt
open Haukkalampi.Core.Result

// Schema for the level format:
// level:
//   size_x: short
//   size_y: short
//   size_z: short
//   tiles: byte[]

let toNbt(level: Level): NbtElement =
    let data = Map [
        "size_x", int16 level.Size.Width |> NbtShort
        "size_y", int16 level.Size.Height |> NbtShort
        "size_z", int16 level.Size.Depth |> NbtShort
        "tiles", List.init (Array.length level.Tiles) (fun i -> sbyte level.Tiles[i]) |> NbtByteArray
    ]
    NbtCompound data

let fromNbt(nbt: NbtElement): Result<Level> =
    match nbt with
    | NbtCompound data ->
        let getShort name =
            match data.TryFind name with
            | Some nbt ->
                match nbt with
                | NbtShort value -> Ok value
                | _ -> Error(WithMessage $"Not a short: {nbt}")
            | None -> Error(WithMessage $"Required field missing: {name}")
        let getByteArray name =
            match data.TryFind name with
            | Some nbt ->
                match nbt with
                | NbtByteArray value -> Ok value
                | _ -> Error(WithMessage $"Not a byte array: {nbt}")
            | None -> Error(WithMessage $"Required field missing: {name}")

        result {
            let! sizeX = getShort "size_x"
            let! sizeY = getShort "size_y"
            let! sizeZ = getShort "size_z"
            let! tiles = getByteArray "tiles"
            let levelSize: LevelSize = { Width = int sizeX; Height = int sizeY; Depth = int sizeZ }
            let level = new Level(levelSize)
            level.Tiles <- listToArray byte tiles
            return level
        }
    | _ -> Error(WithMessage $"Not an NBT compound: {nbt}")
