// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Level.Generator.Feature

open Haukkalampi.Core.Math
open Haukkalampi.Level.Generator

type OreFeature(targetMaterial, ore, chance, pathLength, pathNodeDistance: Picker<int>, blobSize: Picker<int>) =
    inherit NodePathFeature(pathLength)
    override _.MoveNode pos random =
        { X = pos.X + pathNodeDistance random
          Y = pos.Y + pathNodeDistance random
          Z = pos.Z + pathNodeDistance random }

    override _.GenerateNode level pos random = 
        let blobSize = blobSize random

        for dx = -blobSize to blobSize do
            for dy = -blobSize to blobSize do
                for dz = -blobSize to blobSize do
                    let offset = { X = pos.X + dx; Y = pos.Y + dy; Z = pos.Z + dz }
                    if level.IsWithinBounds offset && level.GetTile offset = targetMaterial then
                        let dist = dx * dx + dy * dy + dz * dz |> float32
                        let limit = chance * (1f - dist / float32(blobSize * blobSize))
                        if random.NextSingle() < limit then
                            level.SetTile offset ore

        true
