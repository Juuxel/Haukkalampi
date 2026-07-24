// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Haukkalampi.Protocol.Packet

open Haukkalampi.Core.Io
open Haukkalampi.Core.Result

type C2SPacket =
    | PlayerIdentification of protocolVersion: byte * username: string * verificationKey: string * unused: byte
    | SetBlock of x: int16 * y: int16 * z: int16 * mode: byte * blockType: byte
    | PositionAndOrientation of playerId: byte * x: int16 * y: int16 * z: int16 * yaw: byte * pitch: byte
    | Message of unused: byte * message: string
    static member Decode(input: DataReader) =
        result {
            let! key = input.ReadU8()
            match key with
                | 0x00uy ->
                    let! protocolVersion = input.ReadU8()
                    let! username = input.ReadPaddedUtf8String()
                    let! verificationKey = input.ReadPaddedUtf8String()
                    let! unused = input.ReadU8()
                    return PlayerIdentification(protocolVersion, username, verificationKey, unused)
                | 0x05uy ->
                    let! x = input.ReadI16()
                    let! y = input.ReadI16()
                    let! z = input.ReadI16()
                    let! mode = input.ReadU8()
                    let! blockType = input.ReadU8()
                    return SetBlock(x, y, z, mode, blockType)
                | 0x08uy ->
                    let! playerId = input.ReadU8()
                    let! x = input.ReadI16()
                    let! y = input.ReadI16()
                    let! z = input.ReadI16()
                    let! yaw = input.ReadU8()
                    let! pitch = input.ReadU8()
                    return PositionAndOrientation(playerId, x, y, z, yaw, pitch)
                | 0x0duy ->
                    let! unused = input.ReadU8()
                    let! message = input.ReadPaddedUtf8String()
                    return Message(unused, message)
                | x -> return! Error(WithMessage $"Unknown C2S packet: {x}")
        }

type S2CPacket =
    | ServerIdentification of protocolVersion: byte * serverName: string * serverMotd: string * userType: byte
    | Ping
    | LevelInitialize
    | LevelDataChunk of chunkLength: int16 * chunkData: byte array * percentComplete: byte
    | LevelFinalize of sizeX: int16 * sizeY: int16 * sizeZ: int16
    | SetBlock of x: int16 * y: int16 * z: int16 * blockType: byte
    | SpawnPlayer of playerId: sbyte * playerName: string * x: int16 * y: int16 * z: int16 * yaw: byte * pitch: byte
    | PositionAndOrientation of playerId: sbyte * x: int16 * y: int16 * z: int16 * yaw: byte * pitch: byte
    | PositionAndOrientationUpdate of playerId: sbyte * dx: sbyte * dy: sbyte * dz: sbyte * yaw: byte * pitch: byte
    | PositionUpdate of playerId: sbyte * dx: sbyte * dy: sbyte * dz: sbyte
    | OrientationUpdate of playerId: sbyte * yaw: byte * pitch: byte
    | DespawnPlayer of playerId: sbyte
    | Message of playerId: sbyte * message: string
    | DisconnectPlayer of reason: string
    | UpdateUserType of userType: byte
    member this.Encode(output: DataWriter) =
        result {
            match this with            
            | ServerIdentification(protocolVersion, serverName, serverMotd, userType) ->
                do! output.WriteU8 0x00uy
                do! output.WriteU8 protocolVersion
                do! output.WritePaddedUtf8String serverName
                do! output.WritePaddedUtf8String serverMotd
                do! output.WriteU8 userType
            | Ping ->
                do! output.WriteU8 0x01uy
            | LevelInitialize ->
                do! output.WriteU8 0x02uy
            | LevelDataChunk(chunkLength, chunkData, percentComplete) ->
                do! output.WriteU8 0x03uy
                do! output.WriteI16 chunkLength
                do! output.WritePaddedByteArray chunkData
                do! output.WriteU8 percentComplete
            | LevelFinalize(sizeX, sizeY, sizeZ) ->
                do! output.WriteU8 0x04uy
                do! output.WriteI16 sizeX
                do! output.WriteI16 sizeY
                do! output.WriteI16 sizeZ
            | SetBlock(x, y, z, blockType) ->
                do! output.WriteU8 0x06uy
                do! output.WriteI16 x
                do! output.WriteI16 y
                do! output.WriteI16 z
                do! output.WriteU8 blockType
            | SpawnPlayer(playerId, playerName, x, y, z, yaw, pitch) ->
                do! output.WriteU8 0x07uy
                do! output.WriteI8 playerId
                do! output.WritePaddedUtf8String playerName
                do! output.WriteI16 x
                do! output.WriteI16 y
                do! output.WriteI16 z
                do! output.WriteU8 yaw
                do! output.WriteU8 pitch
            | PositionAndOrientation(playerId, x, y, z, yaw, pitch) ->
                do! output.WriteU8 0x08uy
                do! output.WriteI8 playerId
                do! output.WriteI16 x
                do! output.WriteI16 y
                do! output.WriteI16 z
                do! output.WriteU8 yaw
                do! output.WriteU8 pitch
            | PositionAndOrientationUpdate(playerId, dx, dy, dz, yaw, pitch) ->
                do! output.WriteU8 0x09uy
                do! output.WriteI8 playerId
                do! output.WriteI8 dx
                do! output.WriteI8 dy
                do! output.WriteI8 dz
                do! output.WriteU8 yaw
                do! output.WriteU8 pitch
            | PositionUpdate(playerId, dx, dy, dz) ->
                do! output.WriteU8 0x0auy
                do! output.WriteI8 playerId
                do! output.WriteI8 dx
                do! output.WriteI8 dy
                do! output.WriteI8 dz
            | OrientationUpdate(playerId, yaw, pitch) ->
                do! output.WriteU8 0x0buy
                do! output.WriteI8 playerId
                do! output.WriteU8 yaw
                do! output.WriteU8 pitch
            | DespawnPlayer playerId ->
                do! output.WriteU8 0x0cuy
                do! output.WriteI8 playerId
            | Message(playerId, message) ->
                do! output.WriteU8 0x0duy
                do! output.WriteI8 playerId
                do! output.WritePaddedUtf8String message
            | DisconnectPlayer reason ->
                do! output.WriteU8 0x0euy
                do! output.WritePaddedUtf8String reason
            | UpdateUserType userType ->
                do! output.WriteU8 0x0fuy
                do! output.WriteU8 userType
        }
