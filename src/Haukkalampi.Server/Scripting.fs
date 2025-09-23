module Haukkalampi.Server.Scripting

open Haukkalampi.Core.Math
open Haukkalampi.Level
open Haukkalampi.Tile

type IScriptingServer =
    abstract member TickEvent: IEvent<unit>
    abstract member ScheduleTick: (unit -> unit) -> unit

type ISpreadableParams =
    abstract member Spread: unit -> obj array

[<Struct>]
type Spreadable0 =
    | Empty
    static member Ignoring _: Spreadable0 = Empty
    interface ISpreadableParams with
        member _.Spread() = Array.empty

[<Struct>]
type SpreadableN(values: obj array) =
    interface ISpreadableParams with
        member _.Spread() = values

let functionOf (value: obj) (inputs: obj array): obj =
    IronPython.Runtime.Operations.PythonCalls.Call(value, inputs)

type PyEvent<'T, 'U>(event: IEvent<'T>, conversion: 'T -> 'U) =
    member _.subscribe(func: obj) =
        event.Add(fun data ->
            let converted = conversion data :> obj
            let inputs =
                if converted :? ISpreadableParams then
                    (converted :?> ISpreadableParams).Spread()
                else
                    [| converted |]
            functionOf func inputs |> ignore)

type PyLevel(level: Level) =
    member val tile_changed =
        PyEvent(level.TileChangedEvent, fun(pos, tile) -> SpreadableN [| pos.X; pos.Y; pos.Z; int tile |])

    member _.width = level.Size.Width
    member _.height = level.Size.Height
    member _.depth = level.Size.Depth

    member _.get_tile x y z =
        level.GetTile { X = x; Y = y; Z = z } |> int

    member _.set_tile x y z tile =
        level.SetTile { X = x; Y = y; Z = z } (enum tile)

    member _.is_within_bounds x y z =
        level.IsWithinBounds { X = x; Y = y; Z = z }

type PyServer(server: IScriptingServer) =
    member val tick = PyEvent(server.TickEvent, Spreadable0.Ignoring)
    member _.schedule_tick func =
        server.ScheduleTick(fun() -> functionOf func Array.empty |> ignore)
