namespace Haukkalampi.Octree

type Octree<'T> =
    | Split of Size: int * Children: Octants<'T>
    | Leaf of Size: int * Value: 'T
and Octants<'T> =
    { T000: Octree<'T>
      T100: Octree<'T>
      T010: Octree<'T>
      T001: Octree<'T>
      T110: Octree<'T>
      T101: Octree<'T>
      T011: Octree<'T>
      T111: Octree<'T> }

module Octree =
    let size tree =
        match tree with
        | Split(size, _) -> size
        | Leaf(size, _) -> size

    let rec merge tree: Octree<'a> =
        match tree with
        | Split(size, c) ->
            let t000 = merge c.T000
            let t100 = merge c.T100
            let t010 = merge c.T010
            let t001 = merge c.T001
            let t110 = merge c.T110
            let t101 = merge c.T101
            let t011 = merge c.T011
            let t111 = merge c.T111
            match t000 with
            | Leaf(_, value) when t000 = t100 && t100 = t010 && t010 = t001 && t001 = t110 && t110 = t101 && t101 = t011 && t011 = t111 ->
                Leaf(size, value)
            | _ ->
                let octants =
                    { T000 = t000
                      T100 = t100
                      T010 = t010
                      T001 = t001
                      T110 = t110
                      T101 = t101
                      T011 = t011
                      T111 = t111 }
                Split(size, octants)
        | Leaf(_, _) -> tree

    let inline private checkIndex paramName value tree =
        if value < 0 || value >= size tree then
            let msg = $"Coordinate {value} out of range for octree (0..{size tree - 1} expected)"
            raise(System.ArgumentOutOfRangeException(paramName, msg))

    [<TailCall>]
    let rec get x y z tree =
        checkIndex "x" x tree
        checkIndex "y" y tree
        checkIndex "z" z tree
        match tree with
        | Split(size, children) ->
            let halfSize = size / 2
            let x, y, z, next =
                match x >= halfSize, y >= halfSize, z >= halfSize with
                | false, false, false -> x, y, z, children.T000
                | true, false, false -> x - halfSize, y, z, children.T100
                | false, true, false -> x, y - halfSize, z, children.T010
                | false, false, true -> x, y, z - halfSize, children.T001
                | true, true, false -> x - halfSize, y - halfSize, z, children.T110
                | true, false, true -> x - halfSize, y, z - halfSize, children.T101
                | false, true, true -> x, y - halfSize, z - halfSize, children.T011
                | true, true, true -> x - halfSize, y - halfSize, z - halfSize, children.T111
            get x y z next
        | Leaf(_, value) ->
            value

    let rec set x y z value tree =
        checkIndex "x" x tree
        checkIndex "y" y tree
        checkIndex "z" z tree
        match tree with
        | Leaf(1, _) -> Leaf(1, value)
        | Split(size, children) ->
            let halfSize = size / 2
            match x >= halfSize, y >= halfSize, z >= halfSize with
            | false, false, false -> Split(size, { children with T000 = set x y z value children.T000 })
            | true, false, false -> Split(size, { children with T100 = set (x - halfSize) y z value children.T100 })
            | false, true, false -> Split(size, { children with T010 = set x (y - halfSize) z value children.T010 })
            | false, false, true -> Split(size, { children with T001 = set x y (z - halfSize) value children.T001 })
            | true, true, false -> Split(size, { children with T110 = set (x - halfSize) (y - halfSize) z value children.T110 })
            | true, false, true -> Split(size, { children with T101 = set (x - halfSize) y (z - halfSize) value children.T101 })
            | false, true, true -> Split(size, { children with T011 = set x (y - halfSize) (z - halfSize) value children.T011 })
            | true, true, true -> Split(size, { children with T111 = set (x - halfSize) (y - halfSize) (z - halfSize) value children.T111 })
        | Leaf(size, currentValue) ->
            let halfSize = size / 2
            let octant = Leaf(halfSize, currentValue)
            let octants =
                { T000 = octant
                  T100 = octant
                  T010 = octant
                  T001 = octant
                  T110 = octant
                  T101 = octant
                  T011 = octant
                  T111 = octant }
            Split(size, octants) |> set x y z value

    let toXzyArray transform tree =
        let size = size tree
        let array = Array.zeroCreate(size * size * size)
        for x = 0 to size - 1 do
            for y = 0 to size - 1 do
                for z = 0 to size - 1 do
                    array[x + size * (z + size * y)] <- get x y z tree |> transform
        array

    let toXzyArrayA (transform: 'a -> 'b) (input: 'a[,,]) size =
        let array = Array.zeroCreate(size * size * size)
        for x = 0 to size - 1 do
            for y = 0 to size - 1 do
                for z = 0 to size - 1 do
                    array[x + size * (z + size * y)] <- input[x, y, z] |> transform
        array
