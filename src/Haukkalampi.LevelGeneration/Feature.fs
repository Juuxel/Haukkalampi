namespace Haukkalampi.Level.Generator.Feature

open Haukkalampi.Core.Math
open Haukkalampi.Level.Generator
open Haukkalampi.Tile

type IFeature =
    abstract member Generate: IWritableGeneratingLevel -> BlockPos -> System.Random -> unit

type FlowerFeature(tile) =
    interface IFeature with
        member _.Generate level pos _ =
            level.SetTile pos tile

type TreeFeature(height: Picker<int>, leafRadius: Picker<int>) =
    let setIfAir (level: IWritableGeneratingLevel) pos tile =
        if level.IsWithinBounds pos && level.IsAir pos then
            level.SetTile pos tile

    interface IFeature with
        member _.Generate level pos random =
            let height = height random
            let radius = leafRadius random

            for dy = 0 to height - 1 do
                setIfAir level { pos with Y = pos.Y + dy } Tile.Log

            let pos = { pos with Y = pos.Y + max 1 (height - 2) }

            for dx = -radius to radius do
                for dz = -radius to radius do
                    for dy = 0 to 1 do
                        setIfAir level { X = pos.X + dx; Y = pos.Y + dy; Z = pos.Z + dz } Tile.Leaves
            for dy = 2 to 3 do
                for offset = 0 to min 2 (radius - 1) do
                    setIfAir level { pos with X = pos.X + offset; Y = pos.Y + dy } Tile.Leaves
                    setIfAir level { pos with X = pos.X - offset; Y = pos.Y + dy } Tile.Leaves
                    setIfAir level { pos with Y = pos.Y + dy; Z = pos.Z + offset } Tile.Leaves
                    setIfAir level { pos with Y = pos.Y + dy; Z = pos.Z - offset } Tile.Leaves
