namespace Haukkalampi.Level.Generator.Feature

open Haukkalampi.Core.Math
open Haukkalampi.Level.Generator
open Haukkalampi.Tile

type IFeature =
    abstract member Generate: IWritableGeneratingLevel -> BlockPos -> System.Random -> unit

type FlowerFeature(tile) =
    let canGenerateOn tile =
        tile = Tile.Dirt || tile = Tile.Grass

    interface IFeature with
        member _.Generate level pos _ =
            if level.IsAir pos then
                let belowPos = pos.Down
                if level.IsWithinBounds belowPos && canGenerateOn(level.GetTile belowPos) then
                    level.SetTile pos tile
