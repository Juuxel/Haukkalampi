open Haukkalampi.Core.Io
open Haukkalampi.Core.Math
open Haukkalampi.Level
open Haukkalampi.Level.Generation
open Haukkalampi.Player
open Haukkalampi.Protocol.Packet
open Haukkalampi.Tile
open System.Collections.Generic
open System.IO.Compression
open System.Net
open System.Net.Sockets
open System.Threading

type IServerHandle =
    abstract BroadcastMessage: Player -> string -> unit

type PlayerConnectionState =
    | Start
    | Ready of Player: Player
    | Disconnected

type PlayerConnection(server: IServerHandle, level: Level, playerId: sbyte, socket: Socket) =
    let server = server
    let level = level
    let playerId = playerId
    let socket = socket
    let stream = new NetworkStream(socket)
    let dataReaderWriter = new DataReaderWriterImpl(stream)
    let mutable state = Start

    let disconnect() =
        state <- Disconnected
        stream.Dispose()
        socket.Dispose()

    member _.IsReady =
        state.IsReady

    member _.IsDisconnected =
        state.IsDisconnected

    member _.Player =
        match state with
        | Ready player -> player
        | _ -> failwith "invalid state"

    member this.SendPacket(packet: S2CPacket) =
        match packet.Encode dataReaderWriter with
        | Ok _ -> ()
        | Error err ->
            let msg = $"Could not send packet to player {this.Player.Name} due to {err} - disconnecting!"
            disconnect()
            failwith msg

    member this.SendLevel() =
        this.SendPacket LevelInitialize
        let levelArray = level.Tiles
        use memoryStream = new System.IO.MemoryStream()
        using (new GZipStream(memoryStream, CompressionMode.Compress)) (fun gz ->
            let levelDataLength = Array.length levelArray
            gz.WriteByte(byte(levelDataLength >>> 24 &&& 0xFF))
            gz.WriteByte(byte(levelDataLength >>> 16 &&& 0xFF))
            gz.WriteByte(byte(levelDataLength >>> 8 &&& 0xFF))
            gz.WriteByte(byte(levelDataLength &&& 0xFF))
            gz.Write levelArray)
        let chunks = memoryStream.ToArray() |> Array.chunkBySize 1024
        let chunkCount = Array.length chunks
        for index, chunk in Array.toSeq chunks |> Seq.indexed do
            let percentage = byte(100f * float32 index / float32 chunkCount)
            this.SendPacket(LevelDataChunk(int16(Array.length chunk), chunk, percentage))
        this.SendPacket(LevelFinalize(level.Size.Width |> int16, level.Size.Height |> int16, level.Size.Depth |> int16))

    member private this.InStartState() =
        match C2SPacket.Decode dataReaderWriter with
        | Ok packet ->
            match packet with
            | PlayerIdentification(protocolVersion, username, _, _) ->
                let player: Player =
                    { Id = playerId
                      Name = username
                      X = 0.5f * float32 level.Size.Width
                      Y = 50f
                      Z = 0.5f * float32 level.Size.Depth
                      Yaw = 0f
                      Pitch = 0f }
                level.Players.Add player
                this.SendPacket(ServerIdentification(7uy, "My server", "No motd", 100uy))
                this.SendLevel()
                printfn "player %s connected" username
                let packet = S2CPacket.PositionAndOrientation(
                    -1y,
                    player.X |> FixedPoint.floatToFShort,
                    player.Y |> FixedPoint.floatToFShort,
                    player.Z |> FixedPoint.floatToFShort,
                    0uy,
                    0uy
                )
                this.SendPacket packet
                state <- Ready player
            | _ -> failwith "unidentified player"
        | Error _ ->
            printfn $"Could not establish connection with player"
            disconnect()

    member private _.InReadyState (player: Player) =
        match C2SPacket.Decode dataReaderWriter with
        | Ok packet ->
            match packet with
            | C2SPacket.SetBlock(x, y, z, mode, blockType) ->
                let pos = { X = int x; Y = int y; Z = int z }
                let oldTile = level.GetTile pos
                if oldTile = Tile.Unbreakable then
                    level.SetTile pos Tile.Unbreakable
                else
                    let newTile = if mode = 0uy then Tile.Air else int blockType |> enum
                    level.SetTile pos newTile
            | C2SPacket.PositionAndOrientation(_, x, y, z, yaw, pitch) ->
                player.X <- FixedPoint.fShortToFloat x
                player.Y <- FixedPoint.fShortToFloat y
                player.Z <- FixedPoint.fShortToFloat z
                player.Yaw <- float32 yaw |> mapFloat32 0f 255f -System.Single.Pi System.Single.Pi
                player.Pitch <- float32 pitch |> mapFloat32 0f 255f -System.Single.Pi System.Single.Pi
            | C2SPacket.Message(_, message) ->
                printfn "<%s> %s" player.Name message
                server.BroadcastMessage player message
            | _ -> failwith "invalid packet"
        | Error _ ->
            printfn $"Connection lost with player #{player.Id} {player.Name}"
            disconnect()

    member this.Loop() =
        while not state.IsDisconnected do
            try
                match state with
                | Start -> this.InStartState()
                | Ready player -> this.InReadyState player
            with
                | ex ->
                    if state <> Disconnected then
                        eprintfn $"Player loop suffered an error: {ex} - terminating"
                        state <- Disconnected

type Server(connectedPlayers: IList<PlayerConnection>) =
    let connectedPlayers = connectedPlayers
    let messageQueue = Queue<Player * string>()

    member this.Tick() =
        let toRemove = List<PlayerConnection>()
        for player in connectedPlayers do
            if player.IsReady then
                while messageQueue.Count > 0 do
                    let sender, message = messageQueue.Dequeue()
                    let formattedMessage = $"&5{sender.Name}:&f {message}"
                    player.SendPacket(S2CPacket.Message(sender.Id, formattedMessage))
            elif player.IsDisconnected then
                toRemove.Add player

        for player in toRemove do
            connectedPlayers.Remove player |> ignore

    interface IServerHandle with
        member _.BroadcastMessage player message = 
            messageQueue.Enqueue(player, message)

let launchPlayerConnectionThread server level playerId socket: PlayerConnection =
    let connection = new PlayerConnection(server, level, playerId, socket)
    let thread = new Thread(connection.Loop)
    thread.Start()
    connection

let launchGameThread (server: Server) (connectedPlayers: IList<PlayerConnection>) =
    let run() =
        while true do
            server.Tick()
            Thread.Sleep 100
    let thread = new Thread(run)
    thread.Start()
    printfn "Launched game thread"

[<EntryPoint>]
let main args =
    let s = seq {
        let mutable i = 0
        while i < 10 do
            i <- i + 1
            yield i
            yield! seq { 1..i }
    }

    printfn "Starting server..."
    let level = new Level(LevelSize.Huge)
    let generationParams =
        { NoiseSeed = 123
          RandomSeed = 123
          WaterLevel = 32 }
    let gen = new LevelGenerator(generationParams, level)
    gen.Generate()
    printfn "Generated level"

    use socket = new Socket(SocketType.Stream, ProtocolType.Tcp)
    let endPoint = new IPEndPoint(IPAddress.Parse "127.0.0.1", 7778)
    socket.Bind endPoint
    socket.Listen 10

    let mutable nextPlayerId = 0y
    let connectedPlayers = List<PlayerConnection>()
    let server = new Server(connectedPlayers)
    launchGameThread server connectedPlayers
    level.TileChangedEvent.Add(fun(pos, tile) ->
        for player in connectedPlayers do
            if player.IsReady then
                let packet = S2CPacket.SetBlock(int16 pos.X, int16 pos.Y, int16 pos.Z, byte tile)
                player.SendPacket packet)

    printfn "Listening for players"
    while true do
        let socket = socket.Accept()
        printfn "Initiating connection to player #%d" nextPlayerId
        connectedPlayers.Add(launchPlayerConnectionThread server level nextPlayerId socket)
        nextPlayerId <- nextPlayerId + 1y

    0
