namespace Haukkalampi.Level.Generator.Feature

open Haukkalampi.Core.Math
open Haukkalampi.Level

type IFeature =
    abstract member Generate: Level -> BlockPos -> System.Random -> unit
