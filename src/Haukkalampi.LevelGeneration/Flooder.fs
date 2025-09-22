namespace Haukkalampi.Level.Generator

open Haukkalampi.Core.Math
open Haukkalampi.Level
open Haukkalampi.Tile

open System.Collections.Generic

[<AbstractClass>]
type VisitationQueue<'T>() =
    let known: ISet<'T> = HashSet()
    let toVisit: Queue<'T> = Queue()

    member _.Post(next: 'T) =
        if not(known.Contains next) then
            toVisit.Enqueue next
            known.Add next |> ignore

    member this.VisitAll() =
        while not(toVisit.Count = 0) do
            let next = toVisit.Dequeue()
            this.Visit next

    abstract member Visit: 'T -> unit

type Flooder(level: Level, tile: Tile) =
    inherit VisitationQueue<BlockPos>()

    override this.Visit pos =
        if level.IsWithinBounds pos && level.IsAir pos then
            level.SetTile pos tile
            this.Post pos.North
            this.Post pos.East
            this.Post pos.South
            this.Post pos.West
            this.Post pos.Down
