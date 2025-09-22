namespace Haukkalampi.Level.Generator

open Haukkalampi.Core.Math
open Haukkalampi.Tile

type IGeneratingLevelView =
    abstract member NoiseSeed: int64
    abstract member GetTile: BlockPos -> Tile
    abstract member IsAir: BlockPos -> bool
    abstract member GetTopY: int -> int -> int
    abstract member IsWithinBounds: BlockPos -> bool
