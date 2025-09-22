namespace Haukkalampi.Level.Generator.Feature

open Haukkalampi.Core.Math
open Haukkalampi.Level
open Haukkalampi.Level.Generator

[<AbstractClass>]
type NodePathFeature(pathLength: Picker<int>) =
    abstract member MoveNode: BlockPos -> System.Random -> BlockPos
    abstract member GenerateNode: Level -> BlockPos -> System.Random -> bool

    abstract member Generate: Level * BlockPos * System.Random -> unit
    default this.Generate(level, origin, random) =
        let pathSize = pathLength random
        let path: BlockPos array = Array.create pathSize BlockPos.Zero
        path[0] <- origin
        if pathSize > 1 then
            for i = 1 to pathSize - 1 do
                path[i] <- this.MoveNode path[i - 1] random
        let rec loop i =
            if i < Array.length path then
                let pos = path[i]
                let shouldContinue = this.GenerateNode level pos random
                if shouldContinue then
                    loop(i + 1)
        loop 0

    interface IFeature with
        member this.Generate level origin random =
            this.Generate(level, origin, random)
        
