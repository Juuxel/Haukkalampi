// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Level.Generator.Noise

open Haukkalampi.Core.Math
open Haukkalampi.OpenSimplex2

type NoiseFunction = float -> float -> float

module NoiseFunction =
    let baseLayer seed x z =
        float(OpenSimplex2.Noise2(seed, x, z))

    let absLayer (parent: NoiseFunction) x z =
        parent x z |> abs

    let clampLayer min max (parent: NoiseFunction) x z =
        parent x z |> clamp min max

    let cutoffLayer minCutoff maxCutoff target (parent: NoiseFunction) x z =
        let noise = parent x z
        if minCutoff <= noise && noise <= maxCutoff then
            target
        else
            noise

    let easeInOutLayer exponent min max (parent: NoiseFunction) x z =
        parent x z
        |> norm min max
        |> easeInOut exponent
        |> lerp min max

    let mixLayer (delta: NoiseFunction) (a: NoiseFunction) (b: NoiseFunction) x z =
        lerp (a x z) (b x z) (delta x z)

    let multiplyLayer (a: NoiseFunction) (b: NoiseFunction) x z =
        a x z * b x z

    let octaveLayer level (primary: NoiseFunction) (secondary: NoiseFunction) x z =
        primary x z + secondary (level * x) (level * z) / level

    let offsetLayer offsetX offsetZ (parent: NoiseFunction) x z =
        parent (x + offsetX) (z + offsetZ)

    let outputScaleLayer a1 b1 a2 b2 (parent: NoiseFunction) x z =
        parent x z |> map a1 b1 a2 b2

    let scaleLayer scaleX scaleZ (parent: NoiseFunction) x z =
        parent (scaleX * x) (scaleZ * z)

    let shiftLayer strength (sourceX: NoiseFunction) (sourceZ: NoiseFunction) (parent: NoiseFunction) x z =
        let dx = sourceX x z
        let dz = sourceZ x z
        parent (x + dx * strength) (z + dz * strength)

    let smoothenLayer size (parent: NoiseFunction) x z =
        let xp = x % size
        let zp = z % size
        if xp = 0.0 && zp = 0.0 then
            parent x z
        else
            let x0 = x - xp
            let x1 = x0 + size
            let z0 = z - zp
            let z1 = z0 + size
            let xp = xp / size
            let zp = zp / size
            let n11 = parent x0 z0
            let n21 = parent x1 z0
            let n12 = parent x0 z1
            let n22 = parent x1 z1
            bilerp n11 n21 n12 n22 xp zp

    let subtractLayer (first: NoiseFunction) (second: NoiseFunction) x z =
        first x z - second x z
