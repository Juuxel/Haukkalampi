namespace Haukkalampi.Level.Generator

open Haukkalampi.Core.Math
open Haukkalampi.Level.Generator.Noise
open Haukkalampi.Tile

type IPlacer =
    abstract member Place: IReadableGeneratingLevel -> BlockPos -> System.Random -> BlockPos seq

type ChainedPlacer(placers: IPlacer list) =
    interface IPlacer with
        member _.Place level pos random =
            seq {
                let mutable current = Seq.singleton pos

                for placer in placers do
                    current <- Seq.collect(fun pos -> placer.Place level pos random) current

                yield! current
            }

type SpreadPlacer(distance: Picker<int>) =
    static member InChunk = SpreadPlacer(Picker.uniformInt 0 15)

    interface IPlacer with
        member _.Place _ pos random =
            let next = { pos with X = pos.X + distance random; Z = pos.Z + distance random }
            Seq.singleton next

type ChancePlacer(chance: float32) =
    interface IPlacer with
        member _.Place _ pos random =
            if random.NextSingle() < chance then
                Seq.singleton pos
            else
                Seq.empty

type AnyHeightPlacer private() =
    static member Instance = new AnyHeightPlacer()

    interface IPlacer with
        member _.Place level pos random =
            let next = { pos with Y = random.Next(level.GetTopY pos.X pos.Z) }
            Seq.singleton next

type HeightPlacer(height: Picker<int>, clamp) =
    interface IPlacer with
        member _.Place level pos random =
            let mutable y = height random
            if clamp then
                y <- min y (level.GetTopY pos.X pos.Z)
            let next = { pos with Y = y }
            Seq.singleton next

type RepeatPlacer(count: Picker<int>) =
    interface IPlacer with
        member _.Place _ pos random =
            let count = count random
            Seq.replicate count pos

type TopYPlacer private() =
    static member Instance = new TopYPlacer()

    interface IPlacer with
        member _.Place level pos _ =
            let next = { pos with Y = level.GetTopY pos.X pos.Z + 1 }
            Seq.singleton next

type BoundsFilterPlacer private() =
    static member Instance = new BoundsFilterPlacer()

    interface IPlacer with
        member _.Place level pos _ =
            if level.IsWithinBounds pos then
                Seq.singleton pos
            else
                Seq.empty

type NoiseBasedPlacer(noise: NoiseFunction, min: int, max: int) =
    interface IPlacer with
        member _.Place _ pos _ =
            let noise = noise pos.X pos.Z |> clamp 0 1
            let count = lerp min max noise |> round |> int
            Seq.replicate count pos

type PlantSoilPlacer private() =
    let canGenerateOn tile =
        tile = Tile.Dirt || tile = Tile.Grass

    static member Instance = new PlantSoilPlacer()

    interface IPlacer with
        member _.Place level pos _ =
            let belowPos = pos.Down
            if level.IsAir pos && level.IsWithinBounds belowPos && canGenerateOn(level.GetTile belowPos) then
                Seq.singleton pos
            else
                Seq.empty
