// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Level.Generator.Noise

open Haukkalampi.Core.Math
open Haukkalampi.Level.Generator
open Haukkalampi.OpenSimplex2

type NoiseFunction3D = float -> float -> float -> float

module NoiseFunction3D =
    let private COS_120 = -0.5
    let private SIN_120 = sin(2. * System.Double.Pi / 3.)
    let private COS_240 = COS_120
    let private SIN_240 = -SIN_120

    let adaptingLayer3D (parent: NoiseFunction) x y z =
        let x2 = x + COS_120 * y + COS_240 * z
        let y2 = SIN_120 * y + SIN_240 * z
        parent x2 y2

    let baseLayer3D seed x y z =
        OpenSimplex2.Noise3_ImproveXZ(seed, x, y, z)

    let clampLayer3D min max (parent: NoiseFunction3D) x y z =
        parent x y z |> clamp min max

    let levelBaseLayer3D (level: IReadableGeneratingLevel) (x: float) (y: float) (z: float) =
        let pos = { X = int x; Y = int y; Z = int z }
        if not(level.IsWithinBounds pos) || level.IsAir pos then
            -1.
        else
            1.

    let octaveLayer3D (parent: NoiseFunction3D) x y z =
        parent x y z + parent (2. * x) (2. * y) (2. * z) * 0.5

    let scaleLayer3D scale (parent: NoiseFunction3D) x y z =
        parent (scale * x) (scale * y) (scale * z)

    let shiftLayer3D strengthX strengthY strengthZ (sourceX: NoiseFunction3D) (sourceY: NoiseFunction3D) (sourceZ: NoiseFunction3D) (parent: NoiseFunction3D) x y z =
        let dx = sourceX x y z
        let dy = sourceY x y z
        let dz = sourceZ x y z
        parent (x + dx * strengthX) (y + dy * strengthY) (z + dz * strengthZ)

    let smoothenLayer3D size (parent: NoiseFunction3D) x y z =
        let xp = x % size
        let yp = y % size
        let zp = z % size
        if xp = 0. && yp = 0. && zp = 0. then
            parent x y z
        else
            let x0 = x - xp
            let x1 = x0 + size
            let y0 = y - yp
            let y1 = y0 + size
            let z0 = z - zp
            let z1 = z0 + size
            let xp = xp / size
            let yp = yp / size
            let zp = zp / size
            let n111 = parent x0 y0 z0
            let n211 = parent x1 y0 z0
            let n121 = parent x0 y0 z1
            let n221 = parent x1 y0 z1
            let n112 = parent x0 y1 z0
            let n212 = parent x1 y1 z0
            let n122 = parent x0 y1 z1
            let n222 = parent x1 y1 z1
            trilerp n111 n211 n121 n221 n112 n212 n122 n222 xp zp yp
