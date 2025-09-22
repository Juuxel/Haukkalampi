namespace Haukkalampi.Level.Generator

type Picker<'T> = System.Random -> 'T

module Picker =
    let constant value: Picker<'T> =
        fun _ -> value

    let uniformFloat min max: Picker<float32> =
        fun random -> random.NextSingle() * (max - min) + min

    let uniformInt minInclusive maxInclusive: Picker<int> =
        fun random -> random.Next(minInclusive, maxInclusive + 1)

    let binomial n p: Picker<int> =
        fun random ->
            let mutable result = 0
            for _ = 1 to n do
                if random.NextSingle() < p then
                    result <- result + 1
            result

    let sum (a: Picker<int>) (b: Picker<int>): Picker<int> =
        fun random -> a random + b random
