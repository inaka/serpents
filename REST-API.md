## RESTful API

### Endpoints

#### ``POST /games``
To create games

##### Parameters
All parameters are optional
```json
{ "rows": [# OF ROWS]
, "cols": [# OF COLS]
, "ticktime": [# OF TICKTIME MILLISECONDS]
, "countdown": [# OF COUNTDOWN TICKS]
}
```
##### Responses
* **400 Bad Request** with a descriptive message
* **200 OK** with
```json
{ "id": [MATCH ID]
, "rows": [# OF ROWS]
, "cols": [# OF COLS]
, "ticktime": [# OF TICKTIME MILLISECONDS]
, "countdown": [# OF COUNTDOWN TICKS]
, "serpents": {}
, "state": "created"
, "cells": [ {"row": [ROw #], "col": [COL #], "content": [CONTENT]}
           , {"row": [ROw #], "col": [COL #], "content": [CONTENT]}
           , …
           ] /* CONTENT can be "fruit" or "wall" */
             /* ROW # and COL # are 1-based */
}
```

---

#### ``GET /games``
##### Responses
* **200 OK** with
```json
[ { "id": [MATCH ID]
  , "rows": [# OF ROWS]
  , "cols": [# OF COLS]
  , "ticktime": [# OF TICKTIME MILLISECONDS]
  , "countdown": [# OF COUNTDOWN TICKS]
  , "serpents": { "[PLAYER 1 ID]": {…}
                , "[PLAYER 2 ID]": {…}
                , …
                }
  , "state": [STATE] /* "created", "started" or "finished" */
  , "cells": [ {"row": [ROw #], "col": [COL #], "content": [CONTENT]}
             , {"row": [ROw #], "col": [COL #], "content": [CONTENT]}
             , …
             ] /* CONTENT can be "fruit" or "wall" */
               /* ROW # and COL # are 1-based */
  }
, { … }
]
```
Each serpent will look like:
```json
{ "owner": [PLAYER ID]
, "body": [ [[HEAD ROW #],[HEAD COL #]]
          , [[BODY ROW #],[BODY COL #]]
          , [[BODY ROW #],[BODY COL #]]
          , …
          ]
, "status": [STATUS] /* "dead", "alive" */
}
```

---

#### ``GET /games/:game_id``
##### Responses
* **200 OK** with
```json
{ "id": [MATCH ID]
, "rows": [# OF ROWS]
, "cols": [# OF COLS]
, "ticktime": [# OF TICKTIME MILLISECONDS]
, "countdown": [# OF COUNTDOWN TICKS]
, "serpents": { "[PLAYER 1 ID]": {…}
              , "[PLAYER 2 ID]": {…}
              , …
              }
, "state": [STATE] /* "created", "started" or "finished" */
, "cells": [ {"row": [ROw #], "col": [COL #], "content": [CONTENT]}
           , {"row": [ROw #], "col": [COL #], "content": [CONTENT]}
           , …
           ] /* CONTENT can be "fruit" or "wall" */
             /* ROW # and COL # are 1-based */
}
```
Each serpent will look like:
```json
{ "owner": [PLAYER ID]
, "body": [ [[HEAD ROW #],[HEAD COL #]]
          , [[BODY ROW #],[BODY COL #]]
          , [[BODY ROW #],[BODY COL #]]
          , …
          ]
, "status": [STATUS] /* "dead", "alive" */
}
```

---

#### ``PUT /games/:game_id``
To start a game

##### Parameters
All parameters are required
```json
{"state": "started"}
```
##### Responses
* **400 Bad Request** with a descriptive message
* **200 OK** with the same json object as ``GET /games/:game_id``

---

#### ``DELETE /games/:game_id``
To stop a game

##### Responses
* **204 No Content**
