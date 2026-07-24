// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
