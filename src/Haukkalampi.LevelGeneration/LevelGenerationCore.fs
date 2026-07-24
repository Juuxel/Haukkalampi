// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Level.Generator

open Haukkalampi.Core.Math
open Haukkalampi.Tile

type IReadableGeneratingLevel =
    abstract member NoiseSeed: int64
    abstract member GetTile: BlockPos -> Tile
    abstract member IsAir: BlockPos -> bool
    abstract member GetTopY: int -> int -> int
    abstract member IsWithinBounds: BlockPos -> bool

type IWritableGeneratingLevel =
    inherit IReadableGeneratingLevel
    abstract member SetTile: BlockPos -> Tile -> unit
