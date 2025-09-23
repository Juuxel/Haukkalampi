module Haukkalampi.Core.Collection

let listToArray transform list =
    let len = List.length list
    let target = Array.zeroCreate len
    let rec convert l i =
        match l with
        | x :: xs ->
            target[i] <- transform x
            convert xs (i + 1)
        | _ -> ()
    convert list 0
    target
