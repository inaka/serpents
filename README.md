# Serpents
Multi-Player Game on top of HDP protocol


## The Protocol ##

The UDP protocol consists on 5 different messages types that share a common header with some basic user information. The protocol is defined with [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form)s where ALL_CAPS_IMPLY_CONSTANTS, camelCaseImplyPrimitives and the '&' is used to denote "bitwise or" (super intuitive right?) while '|' denotes that one of multiple options can be chosen.

### The Client Message Header ###

Prefixes all client originated messages

    ClientHeader => Flags MessageID UserTime UserId Message
    Flags => uchar => MessageType
    MessageType => PING | INFO | JOIN | ACTION
    PING => 1
    INFO => 2
    JOIN => 3
    ACTION => 4
    MessageID => ushort
    UserTime => ushort
    UserId => ushort
    Message => PingRequest | GamesInfoRequest | GameInfoRequest

Field     | Description
----------|-------------
Flags     | For now, only used to specify the message type
MessageID | If the command requires a reply, the game will resend this ID to the client. During the game, this ID is expected to be the current tick of the client's game
UserTime  | The game will return this value as is on every response
UserId    | The UserId the game provides when joining a game, 0 otherwise

### The Server Message Header ###

Prefixes all server messages, both server originated and the replies.

    ServerHeader => Flags MessageID Time Message
    Flags => uchar => Success & MessageType
    Success => SUCCEEDED | ERROR
    SUCCEEDED => 128
    ERROR => 0
    MessageType => PING_RESPONSE | INFO_RESPONSE | JOIN_RESPONSE | GAME_UPDATE
    PING_RESPONSE => 1
    INFO_RESPONSE => 2
    JOIN_RESPONSE => 3
    GAME_UPDATE => 4
    MessageID => uint
    Time => ushort
    Message => PingResponse | GamesInfoResponse | GameInfoResponse

Field     | Description
----------|-------------
Flags     | For now, only used to specify the message type
MessageID | The same message ID the user sent if a reply or an ID
Time      | In the case of a response, the value sent by the client, otherwise, this is the current tick

### Ping Request ###

The most simple message there is, it's just the header with no message at all.

    PingRequest => Ø

### Ping Response ###

Also just a header.

    PingResponse => Ø

### Game Info Request ###

If no game is specified, returns a list of the current games being played with some basic information, if a GameId is specified, it returns more information about that specific game.

    GamesInfoRequest => Ø

* An empty message will return information about all games.

    GameInfoRequest => GameId
    GameId => ushort

Field    | Description
---------|-------------
GameId   | The GameId to request info from

### Games Info Response ###

Returns information of all current games.

    GamesInfoResponse => GameCount [GameInfo]
    GameCount => ushort
    GameInfo => GameId TickRate CurrentPlayers MaxPlayers
    GameId => ushort
    TickRate => uchar
    CurrentPlayers => uchar
    MaxPlayers => uchar

Field          | Description
---------------|-------------
GameCount      | The amount of running games
GameId         | The game's id
TickRate       | The game's tick rate
CurrentPlayers | How many players are currently in the game
MaxPlayers     | The maximum amount of players allowed in this game

### Game Info Response ###

Returns information on a single game.

    GameInfoResponse => GameId TickRate Cols Rows CurrentPlayers MaxPlayers [Player]
    GameId => ushort
    TickRate => uchar
    Cols => uchar
    Rows => uchar
    CurrentPlayers => uchar
    MaxPlayers => uchar
    Player => PlayerId Name
    PlayerId => uint
    Name => StringSize bytes
    StringSize => uchar

Field          | Description
---------------|-------------
GameId         | The game's id
TickRate       | The game's tick rate
Cols/Rows      | Indicate the size of the map
CurrentPlayers | How many players are currently in the game
MaxPlayers     | The maximum amount of players allowed in this game
Name           | Any string, this is the player's self chosen name

### Join Request ###

Joins a game.

    JoinRequest => GameId Name
    GameId => ushort
    Name => StringSize bytes
    StringSize => uchar

Field          | Description
---------------|-------------
GameId         | The game's id
Name           | Any string, this is the player's self chosen name

### Join Response ###

Returns either an error or the game info if the join was successful. Note that this relies on the SUCCEEDED flag in the header to help with parsing. If this is not received, the client will still receive the game updates and it's up to the client to request the game info again.

    JoinResponse => Error | Success
    Error => GameId Reason
    GameId => ushort
    Reason => StringSize bytes
    StringSize => uchar
    Success => PlayerID GameInfoResponse

Field              | Description
-------------------|-------------
GameId             | The game's id
Reason             | Any string, this is basically a small user friendly description of the error (usually "full" or "started")
PlayerId           | The ID the game assigned to the client
GameInfoResponse   | The exact same format as the ``Game Info Response``