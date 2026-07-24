// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Haukkalampi.Level.Generator.Shapes

open Haukkalampi.Core.Math

let cuboid origin sizeX sizeY sizeZ: BlockPos seq =
    seq {
        for x = 0 to sizeX - 1 do
            for y = 0 to sizeY - 1 do
                for z = 0 to sizeZ - 1 do
                    yield { X = origin.X + x; Y = origin.Y + y; Z = origin.Z + z }
    }

let private isInsideSphere (origin: BlockPos) (radius: float32) pos =
    float32(origin.SquaredDistanceTo pos) <= radius * radius

let sphere origin (radius: float32): BlockPos seq =
    let size = int(2f * radius + 1f)
    let cuboidOrigin =
        { X = origin.X - int radius
          Y = origin.Y - int radius
          Z = origin.Z - int radius }
    cuboid cuboidOrigin size size size
    |> Seq.filter(isInsideSphere origin radius)
