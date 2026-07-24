// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Tile

open Haukkalampi.Core.Math

type Tile =
    | Air = 0
    | Rock = 1
    | Grass = 2
    | Dirt = 3
    | StoneBrick = 4
    | Wood = 5
    | Bush = 6
    | Unbreakable =  7
    | Water = 8
    | CalmWater = 9
    | Lava = 10
    | CalmLava = 11
    | Sand = 12
    | Gravel = 13
    | GoldOre = 14
    | IronOre = 15
    | CoalOre = 16
    | Log = 17
    | Leaves = 18
    | Sponge = 19
    | Glass = 20
    | RedCloth = 21
    | OrangeCloth = 22
    | YellowCloth = 23
    | ChartreuseCloth = 24
    | GreenCloth = 25
    | SpringGreenCloth = 26
    | CyanCloth = 27
    | CapriCloth = 28
    | UltramarineCloth = 29
    | VioletCloth = 30
    | PurpleCloth = 31
    | MagentaCloth = 32
    | RoseCloth = 33
    | DarkGrayCloth = 34
    | LightGrayCloth = 35
    | WhiteCloth = 36
    | Dandelion = 37
    | Rose = 38
    | BrownMushroom = 39
    | RedMushroom = 40
    | GoldBlock = 41
    | IronBlock = 42
    | DoubleSlab = 43
    | Slab = 44
    | Bricks = 45
    | Tnt = 46
    | Bookshelf = 47
    | MossyCobblestone = 48
    | Obsidian = 49

module Tile =
    let private fullBlockBox: Box = { StartX = 0f; StartY = 0f; StartZ = 0f; EndX = 1f; EndY = 1f; EndZ = 1f }
    let private slabBox: Box = { StartX = 0f; StartY = 0f; StartZ = 0f; EndX = 1f; EndY = 0.5f; EndZ = 1f }

    let collisionBox(tile: Tile) =
        match tile with
        | Tile.Slab -> slabBox
        | _ -> fullBlockBox
