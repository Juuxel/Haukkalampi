module Haukkalampi.Core.Math

let inline clamp a b x =
    max a x |> min b

let norm (min: float) (max: float) (value: float) =
    (value - min) / (max - min)

let lerp (min: float) (max: float) (delta: float) =
    min + (max - min) * delta

let bilerp a11 a21 a12 a22 deltaX deltaY =
    lerp (lerp a11 a21 deltaX) (lerp a12 a22 deltaX) deltaY

let trilerp a111 a211 a121 a221 a112 a212 a122 a222 deltaX deltaY deltaZ =
    lerp (bilerp a111 a211 a121 a221 deltaX deltaY) (bilerp a112 a212 a122 a222 deltaX deltaY) deltaZ

let easeInOut (deg: float) (x: float) =
    if x < 0.5 then
        2.0 ** (deg - 1.0) * x ** deg
    else
        1.0 - 0.5 * (-2.0 * x + 2.0) ** deg

let inline map (startA: float) (endA: float) (startB: float) (endB: float) (x: float) =
    (x - startA) / (endA - startA) * (endB - startB)

let inline mapFloat32 (startA: float32) (endA: float32) (startB: float32) (endB: float32) (x: float32) =
    (x - startA) / (endA - startA) * (endB - startB)

module FixedPoint =
    let floatToFByte(f: float32): sbyte =
        sbyte(clamp -128f 127f (f * 32f))

    let fByteToFloat(b: sbyte): float32 =
        float32 b / 32f

    let floatToFShort(f: float32): int16 =
        int16(clamp -32768f 32767f (f * 32f))

    let fShortToFloat(b: int16): float32 =
        float32 b / 32f

[<Struct>]
type Vec3f =
    { X: float32; Y: float32; Z: float32 }
    member this.SquaredLength =
        this.X * this.X + this.Y * this.Y + this.Z * this.Z

    member this.Length =
        sqrt this.SquaredLength

    member this.Normalized =
        this / this.Length

    static member Zero = { X = 0f; Y = 0f; Z = 0f }

    static member (+) (a, b) =
        { X = a.X + b.X
          Y = a.Y + b.Y
          Z = a.Z + b.Z }
    static member (-) (a, b) =
        { X = a.X - b.X
          Y = a.Y - b.Y
          Z = a.Z - b.Z }
    static member (*) (v, t) =
        { X = v.X * t
          Y = v.Y * t
          Z = v.Z * t }
    static member (/) (v, t) =
        { X = v.X / t
          Y = v.Y / t
          Z = v.Z / t }

[<Struct>]
type Vec3d =
    { X: float; Y: float; Z: float }
    static member (+) (a, b) =
        { X = a.X + b.X
          Y = a.Y + b.Y
          Z = a.Z + b.Z }
    static member (-) (a, b) =
        { X = a.X - b.X
          Y = a.Y - b.Y
          Z = a.Z - b.Z }

[<Struct>]
type BlockPos =
    { X: int; Y: int; Z: int }
    member this.SquaredDistanceTo other =
        let xd = this.X - other.X
        let yd = this.Y - other.Y
        let zd = this.Z - other.Z
        xd * xd + yd * yd + zd * zd

    member this.North =
        { X = this.X
          Y = this.Y
          Z = this.Z - 1 }

    member this.East =
        { X = this.X + 1
          Y = this.Y
          Z = this.Z }

    member this.South =
        { X = this.X
          Y = this.Y
          Z = this.Z + 1 }

    member this.West =
        { X = this.X - 1
          Y = this.Y
          Z = this.Z }

    member this.Up =
        { X = this.X
          Y = this.Y + 1
          Z = this.Z }

    member this.Down =
        { X = this.X
          Y = this.Y - 1
          Z = this.Z }

    static member Zero = { X = 0; Y = 0; Z = 0 }
    static member (+) (a, b) =
        { X = a.X + b.X
          Y = a.Y + b.Y
          Z = a.Z + b.Z }
    static member (-) (a, b) =
        { X = a.X - b.X
          Y = a.Y - b.Y
          Z = a.Z - b.Z }

[<Struct>]
type Box =
    { StartX: float32
      StartY: float32
      StartZ: float32
      EndX: float32
      EndY: float32
      EndZ: float32 }
