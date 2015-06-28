## The Protocol
The HDP protocol consists on 5 different messages types that share a common header with some basic user information. The protocol is defined with [BNF](http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form)s where ALL_CAPS_IMPLY_CONSTANTS, camelCaseImplyPrimitives and the '&' is used to denote "bitwise or" (super intuitive right?) while '|' denotes that one of multiple options can be chosen.

### The Client Message Header ###

Prefixes all client originated messages

    ClientHeader => Flags MessageID ClientTime SerpentId Message
    Flags => uchar => MessageType
    MessageType => PING | INFO | JOIN | ACTION
    PING => 1
    INFO => 2
    JOIN => 3
    ACTION => 4
    MessageID => ushort
    ClientTime => ushort
    SerpentId => ushort
    Message => PingRequest | GamesInfoRequest | GameInfoRequest

Field     | Description
----------|-------------
Flags     | For now, only used to specify the message type
MessageID | If the command requires a reply, the game will resend this ID to the client. During the game, this ID is expected to be the current tick of the client's game
ClientTime| The game will return this value as is on every response
SerpentId | The SerpentId the game provides when joining a game, 0 otherwise

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
MessageID | The same message ID the client sent if a reply or an unique server generated ID
Time      | In the case of a response, the value sent by the client, otherwise, this is the current tick

### Ping Request ###

The most simple message there is, it's just the header with no message at all.

    PingRequest => Ø

### Ping Response ###

Also just a header.

    PingResponse => Ø

### Game Info Request ###

If no game is specified, returns a list of the known games in the server with some basic information, if a GameId is specified, it returns more information about that specific game.

    GamesInfoRequest => Ø

* An empty message will return information about all games.

    GameInfoRequest => GameId
    GameId => ushort

Field    | Description
---------|-------------
GameId   | The GameId to request info from

### Games Info Response ###

Returns information of all known games.

    GamesInfoResponse => GameCount [GameInfo]
    GameCount => ushort
    GameInfo => GameId Name State CurrentSerpents MaxSerpents
    GameId => ushort
    Name => StringSize bytes
    StringSize => uchar
    State => uchar => CREATED | COUNTDOWN | STARTED | FINISHED
    CREATED => 0
    COUNTDOWN => 1
    STARTED => 2
    FINISHED => 4
    CurrentSerpents => uchar
    MaxSerpents => uchar

Field           | Description
----------------|-------------
GameCount       | The amount of running games
GameId          | The game's id
Name            | The game's name as it should appear on screens
State           | The game state
CurrentSerpents | How many serpents are currently in the game
MaxSerpents     | The maximum amount of serpents allowed in this game

### Game Info Response ###

Returns information on a single game.

    GameInfoResponse => GameId Name State Flags Cols Rows TickRate Countdown Rounds InitialFood MaxSerpents CurrentSerpents [Serpent]
    GameId => ushort
    State => uchar => CREATED | COUNTDOWN | STARTED | FINISHED
    CREATED => 0
    COUNTDOWN => 1
    STARTED => 2
    FINISHED => 4
    Flags => uchar => Walls & FoodVariation
    Walls => WITH_WALLS | NO_WALLS
    WITH_WALLS => 1
    NO_WALLS => 0
    FoodVariation => NONE | RANDOM | LINEAR | RANDOM_INCREMENT
    NONE => 0
    RANDOM => 2
    LINEAR => 4
    RANDOM_INCREMENT => 6
    Cols => uchar
    Rows => uchar
    TickRate => uchar
    Countdown => uchar
    Rounds => uint
    InitialFood => uchar
    MaxSerpents => uchar
    CurrentSerpents => uchar
    Serpent => SerpentID Name
    SerpentID => uint
    Name => StringSize bytes
    StringSize => uchar

Field           | Description
----------------|-------------
GameId          | The game's id
State           | The game state
Cols/Rows       | Indicate the size of the map
TickRate        | The game's tick rate (i.e. the # of updates per second, regardless of the actual game speed)
Countdown       | The game's countdown length (in ticks)
Rounds          | The game max length (in ticks) - `0` means `no limit`
InitialFood     | How much food is in the serpents belly when they join
MaxSerpents     | The maximum amount of serpents allowed in this game
CurrentSerpents | How many serpents are currently in the game
Name            | Any string, this is either the game or the serpent name

### Join Request ###

Joins a game.

    JoinRequest => GameId Name
    GameId => ushort
    Name => StringSize bytes
    StringSize => uchar

Field          | Description
---------------|-------------
GameId         | The game's id
Name           | Any string, this will be the serpent name

### Join Response ###

Returns either an error or the game info if the join was successful. Note that this relies on the SUCCEEDED flag in the header to help with parsing. If this is not received, the client will still receive the game updates and it's up to the client to request the game info again.

    JoinResponse => Error | Success
    Error => GameId Reason
    GameId => ushort
    Reason => StringSize bytes
    StringSize => uchar
    Success => SerpentID GameInfoResponse
    SerpentID => uint

Field              | Description
-------------------|-------------
GameId             | The game's id
Reason             | Any string, this is basically a small user friendly description of the error (usually "full" or "started")
SerpentId          | The ID the game assigned to the client
GameInfoResponse   | The exact same format as the ``Game Info Response``

### Client Update ###

Notifies the server that the player moved its serpent and informs the last update that was received.

    ClientUpdate => LastServerTick Action
    LastServerTick => ushort
    Action => uchar

Field            | Description
-----------------|-------------
LastServerTick   | Should be the last tick received from the server
Action           | A direction change, set the char to 1 for left, 2 for right, 4 for up and 8 for down, any other value is ignored

### Server Update ###

Sent every server tick (50 times per second) with any updates the game had.

    ServerUpdate => Tick NumDiffs [GameDiff]
    Tick => ushort
    NumDiffs => uchar
    GameDiff => DiffType DiffData
    DiffType => uchar => STATE | COUNTDOWN | ROUNDS | SERPENTS | FRUIT
    STATE => 0
    COUNTDOWN => 1
    ROUNDS => 2
    SERPENTS => 3
    FRUIT => 4
    DiffData => State | Countdown | Rounds | Serpents | Fruit
    State => uchar => CREATED | COUNTDOWN | STARTED | FINISHED
    CREATED => 0
    COUNTDOWN => 1
    STARTED => 2
    FINISHED => 4
    Countdown => ushort
    Rounds => uint
    Serpents => NumSerpents [Serpent]
    NumSerpents => uchar
    Serpent => SerpentID BodyLength [Cell]
    SerpentID => uint
    BodyLength => ushort
    Fruit => Food Cell
    Food => uchar
    Cell => Row Col
    Row => uchar
    Col => uchar

Field     | Description
----------|-------------
Tick      | Am increasing value the client should use as `LastServerTick`
NumDiffs  | Number of `GameDiff`s included in this update
State     | The game state
Countdown | Countdown rounds until the game starts
Rounds    | # of Rounds until the game ends
Food      | How many new cells will be added to the serpent body when a serpent eats this fruit
Row       | 1-based row index
Col       | 1-based col index
