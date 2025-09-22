module Haukkalampi.Level.Generation.Noise.Builtin

open Haukkalampi.Level.Generator
open NoiseFunction
open NoiseFunction3D

let private createRiverLayer(baseLayer: NoiseFunction): NoiseFunction =
    scaleLayer 0.0045 0.0045 baseLayer
    |> absLayer
    |> cutoffLayer 0.135 1 1
    |> smoothenLayer 4
    |> easeInOutLayer 5 0 1
    |> outputScaleLayer 0 1 1 0
    |> smoothenLayer 8

let createBaseLayer seed: NoiseFunction =
    let baseLayer = NoiseFunction.baseLayer seed
    let layer = baseLayer |> scaleLayer 0.013 0.013
    let layer = octaveLayer 2 layer layer
    let minor = offsetLayer 1024 1024 layer
    let layer =
        octaveLayer 5 layer minor
        |> clampLayer -1 1
        |> easeInOutLayer 1.2 -1 1
        |> multiplyLayer (baseLayer |> scaleLayer 0.004 0.004 |> absLayer)
    let mixDelta = scaleLayer 0.13 0.1 baseLayer |> outputScaleLayer -1 1 0 1
    let offset = offsetLayer -2048 4192 layer
    let layer = mixLayer mixDelta layer offset |> smoothenLayer 8
    let river = createRiverLayer baseLayer
    let shiftSource = scaleLayer 0.02 0.02 baseLayer
    let shiftX = offsetLayer -1024 1024 shiftSource
    let shiftZ = offsetLayer 1024 -1024 shiftSource
    subtractLayer layer river
    |> clampLayer -1 1
    |> shiftLayer 11 shiftX shiftZ

let createWarpLayer (level: IGeneratingLevelView) seed: NoiseFunction3D =
    let shiftSource = baseLayer seed |> scaleLayer 0.02 0.02
    let shiftX = shiftSource |> offsetLayer -1024 1024 |> adaptingLayer3D
    let shiftY = shiftSource |> offsetLayer -4192 2048 |> adaptingLayer3D
    let shiftZ = shiftSource |> offsetLayer 1024 -1024 |> adaptingLayer3D

    levelBaseLayer3D level
    |> smoothenLayer3D 4
    |> shiftLayer3D 11 4 11 shiftX shiftY shiftZ
