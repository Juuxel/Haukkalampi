// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Haukkalampi.Core.Text

type TextColor =
    | Black
    | DarkBlue
    | DarkGreen
    | Teal
    | DarkRed
    | Purple
    | DarkYellow
    | Gray
    | DarkGray
    | Indigo
    | BrightGreen
    | Cyan
    | Red
    | Pink
    | Yellow
    | White
    member this.FormattingCode =
        match this with
        | Black -> "&0"
        | DarkBlue -> "&1"
        | DarkGreen -> "&2"
        | Teal -> "&3"
        | DarkRed -> "&4"
        | Purple -> "&5"
        | DarkYellow -> "&6"
        | Gray -> "&7"
        | DarkGray -> "&8"
        | Indigo -> "&9"
        | BrightGreen -> "&a"
        | Cyan -> "&b"
        | Red -> "&c"
        | Pink -> "&d"
        | Yellow -> "&e"
        | White -> "&f"

type Text =
    | Literal of Value: string
    | Sequence of Children: Text list
    | Styled of Text: Text * Color: TextColor
    member this.WithStyle color =
        match this with
        | Styled(text, _) -> Styled(text, color)
        | _ -> Styled(this, color)

    member this.Append other =
        match this with
        | Sequence children -> Sequence(List.append children [other])
        | _ -> Sequence [this; other]

module Text =
    let display (text: Text): string =
        let rec inner color text =
            match text with
            | Literal value -> value
            | Sequence children -> children |> List.toSeq |> Seq.map(inner color) |> Seq.reduce (+)
            | Styled(text, innerColor) -> $"{innerColor.FormattingCode}{inner innerColor text}{color.FormattingCode}"
        inner White text
