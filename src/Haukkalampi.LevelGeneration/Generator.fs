// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Level.Generator

open Haukkalampi.Core.Math
open Haukkalampi.Level
open Haukkalampi.Level.Generator
open Haukkalampi.Level.Generator.Feature
open Haukkalampi.Tile

type LevelGenerationParameters =
    { NoiseSeed: int64
      RandomSeed: int
      WaterLevel: int }

type LevelGenerator(parameters: LevelGenerationParameters, level: Level) =
    let WARP_GRID_HORIZONTAL_SIZE = 4
    let random = new System.Random(parameters.RandomSeed)
    let heights: int[,] = Array2D.zeroCreate level.Size.Width level.Size.Depth

    let recalculateHeights() =
        for x = 0 to level.Size.Width - 1 do
            for z = 0 to level.Size.Depth - 1 do
                heights[x, z] <- level.GetTopY x z

    member private this.GenerateFeatures(features: List<IFeature * IPlacer>) =
        for x in 0..16..(level.Size.Width - 1) do
            for z in 0..16..(level.Size.Depth - 1) do
                let origin = { X = x; Y = 0; Z = z }
                for feature, placer in features do
                    let positions = placer.Place this origin random
                    for pos in positions do
                        feature.Generate this pos random

    member private _.Shape() =
        let noiseLayer =
            Noise.Builtin.createBaseLayer parameters.NoiseSeed
            |> Noise.NoiseFunction.outputScaleLayer -1 1
                (float parameters.WaterLevel - float level.Size.Height * 0.1)
                (float parameters.WaterLevel + float level.Size.Height * 0.35)
        for x = 0 to level.Size.Width - 1 do
            for z = 0 to level.Size.Depth - 1 do
                let y = noiseLayer x z |> int
                heights[x, z] <- y
                for i = 0 to y do
                    level.SetTile { X = x; Y = i; Z = z } Tile.Rock

    member this.Warp() =
        let newTiles = Array.zeroCreate<byte>(level.Size.Width * level.Size.Height * level.Size.Depth)
        let noiseFunction = Noise.Builtin.createWarpLayer this parameters.NoiseSeed
        for x in 0..WARP_GRID_HORIZONTAL_SIZE..(level.Size.Width - 1) do
            for z in 0..WARP_GRID_HORIZONTAL_SIZE..(level.Size.Depth - 1) do
                for y in 0..(level.Size.Height - 1) do
                    let n11 = noiseFunction x y z
                    let n21 = noiseFunction (float(x + WARP_GRID_HORIZONTAL_SIZE)) y z
                    let n12 = noiseFunction x y (float(z + WARP_GRID_HORIZONTAL_SIZE))
                    let n22 = noiseFunction (float(x + WARP_GRID_HORIZONTAL_SIZE)) y (float(z + WARP_GRID_HORIZONTAL_SIZE))
                    for xo = 0 to WARP_GRID_HORIZONTAL_SIZE - 1 do
                        for zo = 0 to WARP_GRID_HORIZONTAL_SIZE - 1 do
                            if x + xo < level.Size.Width && z + zo < level.Size.Depth then
                                let noise = bilerp n11 n21 n12 n22 (float xo / float WARP_GRID_HORIZONTAL_SIZE) (float zo / float WARP_GRID_HORIZONTAL_SIZE)
                                let tile =
                                    if noise < 0 then
                                        Tile.Air
                                    else
                                        Tile.Rock
                                newTiles[level.PackCoords (x + xo) y (z + zo)] <- byte tile
        level.Tiles <- newTiles

    member private this.Carve() =
        this.GenerateFeatures [
            CaveFeature(Picker.uniformInt 40 90, Picker.constant 2, Picker.uniformFloat 1.6f 3f),
            ChainedPlacer [
                ChancePlacer 0.6f
                SpreadPlacer.InChunk
                AnyHeightPlacer.Instance
            ]
        ]

    member private _.Soil() =
        for x = 0 to level.Size.Width - 1 do
            for z = 0 to level.Size.Depth - 1 do
                let y = heights[x, z]
                let depth = 1 + random.Next 2
                for i = 0 to depth do
                    let pos = { X = x; Y = y - i; Z = z }
                    if y - i >= 0 && not(level.IsAir pos) then
                        let tile =
                            if i = 0 then
                                Tile.Grass
                            else
                                Tile.Dirt
                        level.SetTile pos tile

    member private _.Flood() =
        let flooder = Flooder(level, Tile.CalmWater)
        for x = 0 to level.Size.Width - 1 do
            for z = 0 to level.Size.Depth - 1 do
                let topY = heights[x, z]
                if topY <= parameters.WaterLevel then
                    let waterLevelPos = { X = x; Y = parameters.WaterLevel; Z = z }
                    if level.IsAir waterLevelPos then
                        flooder.Post waterLevelPos
                        flooder.VisitAll()
                    let pos = { X = x; Y = topY; Z = z }
                    level.SetTile pos Tile.Sand

    member private this.Plant() =
        let flowerPlacer count =
            ChainedPlacer [
                ChancePlacer 0.4f
                SpreadPlacer.InChunk
                RepeatPlacer count
                SpreadPlacer(Picker.uniformInt -3 3)
                BoundsFilterPlacer.Instance
                TopYPlacer.Instance
                PlantSoilPlacer.Instance
            ]
        this.GenerateFeatures [
            FlowerFeature Tile.Rose, flowerPlacer(Picker.uniformInt 4 7)
            FlowerFeature Tile.Dandelion, flowerPlacer(Picker.uniformInt 6 10)

            TreeFeature(Picker.uniformInt 3 5, Picker.sum (Picker.binomial 1 0.85f) (Picker.constant 1)),
            ChainedPlacer [
                NoiseBasedPlacer(Noise.Builtin.createForestLayer parameters.NoiseSeed, 0, 3)
                SpreadPlacer.InChunk
                TopYPlacer.Instance
                PlantSoilPlacer.Instance
            ]
        ]

    member private this.HideTreasures() =
        let orePlacer chance height =
            ChainedPlacer [
                RepeatPlacer(Picker.uniformInt 1 6)
                ChancePlacer chance
                SpreadPlacer.InChunk
                HeightPlacer(height, true)
            ]
        this.GenerateFeatures [
            OreFeature(Tile.Rock, Tile.CoalOre, 0.9f, Picker.uniformInt 2 7, Picker.uniformInt 3 5, Picker.uniformInt 2 4),
            orePlacer 0.8f (Picker.uniformInt 20 50)

            OreFeature(Tile.Rock, Tile.IronOre, 0.8f, Picker.uniformInt 3 8, Picker.uniformInt 3 5, Picker.uniformInt 2 3),
            orePlacer 0.7f (Picker.uniformInt 10 32)

            OreFeature(Tile.Rock, Tile.GoldOre, 0.7f, Picker.uniformInt 1 4, Picker.uniformInt 3 5, Picker.uniformInt 2 3),
            orePlacer 0.4f (Picker.uniformInt 0 27)
        ]

    member this.Generate() =
        printfn "=== Generating level ==="
        printfn "Noise Seed: %d" parameters.NoiseSeed
        printfn "Random Seed: %d" parameters.RandomSeed
        printfn "Level Size: %dx%dx%d" level.Size.Width level.Size.Height level.Size.Depth
        printfn "Water Level: %d" parameters.WaterLevel
        printfn "========================"
        printfn "Shaping..."
        this.Shape()
        printfn "Warping..."
        this.Warp()
        recalculateHeights()
        printfn "Carving..."
        this.Carve()
        printfn "Soiling..."
        this.Soil()
        recalculateHeights()
        printfn "Flooding..."
        this.Flood()
        printfn "Planting..."
        this.Plant()
        printfn "Hiding treasures..."
        this.HideTreasures()

    interface IWritableGeneratingLevel with
        member _.NoiseSeed = parameters.NoiseSeed
        member _.GetTile pos = level.GetTile pos
        member _.IsAir pos = level.IsAir pos
        member _.GetTopY x z =
            Level.CheckIndex "x" x (Array2D.length1 heights)
            Level.CheckIndex "z" z (Array2D.length2 heights)
            heights[x, z]
        member _.IsWithinBounds pos = level.IsWithinBounds pos
        member _.SetTile pos tile = level.SetTile pos tile
