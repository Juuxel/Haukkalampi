namespace Haukkalampi.Level

open Haukkalampi.Core.Math
open Haukkalampi.Octree
open Haukkalampi.Player
open Haukkalampi.Tile
open System.Collections.Generic

type LevelSize =
    | Tiny
    | Small
    | Normal
    | Huge
    | Massive
    member this.AsInt =
        match this with
        | Tiny -> 64
        | Small -> 128
        | Normal -> 256
        | Huge -> 512
        | Massive -> 1024

    member this.Width = this.AsInt
    member _.Height = 64
    member this.Depth = this.AsInt

type Level(size: LevelSize) =
    let mutable tiles: byte array = Array.zeroCreate (size.Width * size.Height * size.Depth)
    let mapIndex x y z =
        x + size.Width * (z + size.Depth * y)
    let tileChanged = new Event<BlockPos * Tile>()
    let checkIndex paramName value size =
        if value < 0 || value >= size then
            let msg = $"Coordinate {value} out of level bounds (0..{size - 1} expected)"
            raise(System.ArgumentOutOfRangeException(paramName, msg))

    [<CLIEvent>]
    member _.TileChangedEvent = tileChanged.Publish
    member _.Size = size
    member _.Tiles
        with get() = tiles
        and set value = tiles <- value
    member val Players: IList<Player> = List()

    member _.IsWithinBounds(pos: BlockPos): bool =
        0 <= pos.X && pos.X < size.Width && 0 <= pos.Y && pos.Y < size.Height && 0 <= pos.Z && pos.Z < size.Depth

    member _.GetTile(pos: BlockPos): Tile =
        checkIndex "x" pos.X size.Width
        checkIndex "y" pos.Y size.Height
        checkIndex "z" pos.Z size.Depth
        tiles[mapIndex pos.X pos.Y pos.Z] |> int |> enum

    member _.SetTile (pos: BlockPos) (tile: Tile) =
        checkIndex "x" pos.X size.Width
        checkIndex "y" pos.Y size.Height
        checkIndex "z" pos.Z size.Depth
        tiles[mapIndex pos.X pos.Y pos.Z] <- byte tile
        tileChanged.Trigger(pos, tile)

    member this.IsAir(pos: BlockPos): bool =
        this.GetTile pos = Tile.Air

    member _.PackCoords x y z =
        mapIndex x y z

    member this.GetTopY x z =
        let rec inner y =
            if y = 0 then
                0
            elif this.IsAir { X = x; Y = y; Z = z } then
                inner(y - 1)
            else
                y
        inner(size.Height - 1)
