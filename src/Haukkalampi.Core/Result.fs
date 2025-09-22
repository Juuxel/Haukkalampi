module Haukkalampi.Core.Result

type ResultBuilder<'E> internal() =
    member _.Bind(input: Result<'T, 'E>, transform: 'T -> Result<'U, 'E>): Result<'U, 'E> =
        Result.bind transform input

    member _.Return(value: 'T): Result<'T, 'E> =
        Ok value

    member _.ReturnFrom(value: Result<'T, 'E>): Result<'T, 'E> =
        value

    member _.Zero(): Result<unit, 'E> = Ok(())

    member _.For(ts: seq<'T>, transform: 'T -> Result<unit, 'E>): Result<unit, 'E> =
        let rec inner (e: System.Collections.Generic.IEnumerator<'T>) =
            if e.MoveNext() then
                match transform e.Current with
                | Ok() -> inner e
                | Error e -> Error e
            else
                Ok()
        ts.GetEnumerator() |> inner

    member _.Combine(left: Result<unit, 'E>, right: unit -> Result<'T, 'E>): Result<'T, 'E> =
        match left with
        | Ok () -> right()
        | Error e -> Error e

    member _.Delay x = x

    member _.Run(fn: unit -> Result<'T, 'E>): Result<'T, 'E> =
        fn()

    member this.While(condition: unit -> bool, action: unit -> Result<unit, 'E>): Result<unit, 'E> =
        if condition() then
            this.Bind(action(), fun() -> this.While(condition, action))
        else
            this.Zero()

let result<'E> = new ResultBuilder<'E>()

let mergeResults (fn: 'T -> Result<'U, 'E>) (inputs: 'T list): Result<'U list, 'E> =
    let rec inner fn inputs acc =
        match inputs with
        | [x] ->
            match fn x with
            | Ok value -> Ok(List.append acc [value])
            | Error err -> Error err
        | x :: rest ->
            match fn x with
            | Ok value -> inner fn rest (List.append acc [value])
            | Error err -> Error err
        | [] -> invalidArg "inputs" "Cannot merge empty list of inputs"
    inner fn inputs []
