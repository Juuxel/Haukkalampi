// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Player

open Haukkalampi.Core.Math

type Player =
    { Id: sbyte
      Name: string
      mutable X: float32
      mutable Y: float32
      mutable Z: float32
      mutable Yaw: float32
      mutable Pitch: float32 }
    member this.BlockPos: BlockPos =
        { X = int this.X
          Y = int this.Y
          Z = int this.Z }
