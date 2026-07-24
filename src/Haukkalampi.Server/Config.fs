// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

namespace Haukkalampi.Server.Config

open Haukkalampi.Level
open Tommy

module private TomlUtil =
    let tomlIntOf x =
        let int = TomlInteger()
        int.Value <- x
        int

type LevelConfig =
    { Seed: int
      Size: LevelSize }
    member this.WriteToml(table: TomlTable) =
        table["seed"] <- TomlUtil.tomlIntOf this.Seed
        table["size"] <-
            let array = TomlArray()
            array.Add(TomlUtil.tomlIntOf this.Size.Width)
            array.Add(TomlUtil.tomlIntOf this.Size.Height)
            array.Add(TomlUtil.tomlIntOf this.Size.Depth)
            array

type ServerConfig =
    { Port: int
      Levels: LevelConfig list }
    member this.WriteToml(table: TomlTable) =
        table["port"] <- TomlUtil.tomlIntOf this.Port
        table["levels"] <-
            let array = TomlArray()
            array.IsTableArray <- true
            for level in this.Levels do
                let childToml = TomlTable()
                level.WriteToml childToml
                array.Add childToml
            array
