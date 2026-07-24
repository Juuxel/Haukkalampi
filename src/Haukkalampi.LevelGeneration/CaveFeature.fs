// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Level.Generator.Feature

open Haukkalampi.Core.Math
open Haukkalampi.Level.Generator
open Haukkalampi.Tile

type CaveFeature(pathLength, pathNodeDistance: Picker<int>, radius: Picker<float32>) =
    inherit NodePathFeature(pathLength)
    let Y_STRENGTH = 0.85f
    let mutable direction = Vec3f.Zero

    override _.MoveNode pos random: BlockPos =
        // Adjust direction
        let rawDirection: Vec3f =
            { X = random.NextSingle() - 0.5f
              Y = Y_STRENGTH * (random.NextSingle() - 0.5f)
              Z = random.NextSingle() - 0.5f }
        direction <- rawDirection.Normalized

        let distance = pathNodeDistance random |> float32
        { X = pos.X + int(distance * direction.X)
          Y = pos.Y + int(distance * direction.Y)
          Z = pos.Z + int(distance * direction.Z) }

    override _.GenerateNode level pos random: bool =
        let sphere = Shapes.sphere pos (radius random)
        for cavePos in sphere do
            if level.IsWithinBounds cavePos && level.GetTile cavePos = Tile.Rock then
                level.SetTile cavePos Tile.Air
        true

    override _.Generate(level, pos, random) =
        let rawDirection: Vec3f =
            { X = 2f * random.NextSingle() - 1f
              Y = Y_STRENGTH * (2f * random.NextSingle() - 1f)
              Z = 2f * random.NextSingle() - 1f }
        direction <- rawDirection.Normalized
        base.Generate(level, pos, random)
