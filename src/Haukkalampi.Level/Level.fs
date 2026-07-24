// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Level

open Haukkalampi.Core.Math
open Haukkalampi.Player
open Haukkalampi.Tile
open System.Collections.Generic

type LevelSize =
    { Width: int
      Height: int
      Depth: int }
    static let create size =
        { Width = size; Height = 64; Depth = size }
    static member Tiny = create 64
    static member Small = create 128
    static member Normal = create 256
    static member Huge = create 512
    static member Massive = create 1024

type Level(size: LevelSize) =
    let mutable tiles: byte array = Array.zeroCreate (size.Width * size.Height * size.Depth)
    let mapIndex x y z =
        x + size.Width * (z + size.Depth * y)
    let tileChanged = new Event<BlockPos * Tile * Tile>()
    let neighborChanged = new Event<BlockPos * Direction * Tile>()

    static member CheckIndex paramName value size =
        if value < 0 || value >= size then
            let msg = $"Coordinate {value} out of level bounds (0..{size - 1} expected)"
            raise(System.ArgumentOutOfRangeException(paramName, msg))

    static member CheckIndices (pos: BlockPos) (size: LevelSize) =
        Level.CheckIndex "x" pos.X size.Width
        Level.CheckIndex "y" pos.Y size.Height
        Level.CheckIndex "z" pos.Z size.Depth

    [<CLIEvent>]
    member _.TileChangedEvent = tileChanged.Publish
    member _.NeighborChangedEvent = neighborChanged.Publish
    member _.Size = size
    member _.Tiles
        with get() = tiles
        and set value = tiles <- value
    member val Players: IList<Player> = List()

    member _.IsWithinBounds(pos: BlockPos): bool =
        0 <= pos.X && pos.X < size.Width && 0 <= pos.Y && pos.Y < size.Height && 0 <= pos.Z && pos.Z < size.Depth

    member _.GetTile(pos: BlockPos): Tile =
        Level.CheckIndices pos size
        tiles[mapIndex pos.X pos.Y pos.Z] |> int |> enum

    member _.SetTile (pos: BlockPos) (tile: Tile) =
        Level.CheckIndices pos size
        let index = mapIndex pos.X pos.Y pos.Z
        let oldTile = tiles[index] |> int |> enum
        tiles[index] <- byte tile
        tileChanged.Trigger(pos, oldTile, tile)

        for side in Direction.Values do
            neighborChanged.Trigger(pos.Offset side, side.Opposite, tile)

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
