## RESTful API

### Endpoints

#### ``POST /api/games``
To create games

##### Parameters
All parameters are optional
```json
{ "rows": [#-OF-ROWS]
, "cols": [#-OF-COLS]
, "ticktime": [#-OF-TICKTIME-MILLISECONDS]
, "countdown": [#-OF-COUNTDOWN-TICKS]
, "rounds": [MAX-#-OF-GAME-TICKS]
, "initial_food": [INITIAL-FOOD-COUNT]
}
```
##### Responses
* **400 Bad Request** with a descriptive message
* **201 Created** with
```json
{ "id": [MATCH-ID]
, "rows": [#-OF-ROWS]
, "cols": [#-OF-COLS]
, "ticktime": [#-OF-TICKTIME-MILLISECONDS]
, "countdown": [#-OF-COUNTDOWN-TICKS]
, "rounds": [MAX-#-OF-GAME-TICKS]
, "initial_food": [INITIAL-FOOD-COUNT]
, "serpents": {}
, "state": "created"
, "cells": [ {"row": [ROW#], "col": [COL#], "content": [CONTENT]}
           , {"row": [ROW#], "col": [COL#], "content": [CONTENT]}
           , …
           ]
}
```
* `[CONTENT]` can be `"fruit"` or `"wall"`
* `[ROW#]` and `[COL#]` are 1-based

---

#### ``GET /api/games``
##### Responses
* **200 OK** with
```json
[ { "id": [ID]
  , "rows": [#-OF-ROWS]
  , "cols": [#-OF-COLS]
  , "ticktime": [#-OF-TICKTIME-MILLISECONDS]
  , "countdown": [#-OF-COUNTDOWN-TICKS]
  , "rounds": [MAX-#-OF-GAME-TICKS]
  , "initial_food": [INITIAL-FOOD-COUNT]
  , "serpents": [{…}, {…}]
  , "state": [STATE]
  , "cells": [ {"row": [ROW#], "col": [COL#], "content": [CONTENT]}
             , {"row": [ROW#], "col": [COL#], "content": [CONTENT]}
             , …
             ]
  }
, { … }
]
```
* `[STATE]` can be `"created"`, `"countdown"`, `"started"` or `"finished"`
* `[CONTENT]` can be `"fruit"` or `"wall"`
* Each serpent will look like:
```json
{ "name": [NAME]
, "body": [ [[HEAD-ROW#],[HEAD-COL#]]
          , [[ROW#],[COL#]]
          , [[ROW#],[COL#]]
          , …
          ]
, "status": [STATUS]
}
```
* `[STATUS]` can be `"dead"` or `"alive"`

---

#### ``GET /api/games/:game_id``
##### Responses
* **200 OK** with
```json
{ "id": [ID]
, "rows": [#-OF-ROWS]
, "cols": [#-OF-COLS]
, "ticktime": [#-OF-TICKTIME-MILLISECONDS]
, "countdown": [#-OF-COUNTDOWN-TICKS]
, "rounds": [MAX-#-OF-GAME-TICKS]
, "initial_food": [INITIAL-FOOD-COUNT]
, "serpents": [{…}, {…}]
, "state": [STATE]
, "cells": [ {"row": [ROW#], "col": [COL#], "content": [CONTENT]}
           , {"row": [ROW#], "col": [COL#], "content": [CONTENT]}
           , …
           ]
}
```

---

#### ``PUT /api/games/:game_id``
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

#### ``DELETE /api/games/:game_id``
To stop a game

##### Responses
* **204 No Content**

---

#### ``GET /api/games/:game_id/news``
This endpoint returns a general state of the game as a first event and, if the caller keeps the connection open, it will keep sending new events through it as they happen. This endpoint implements [Server Sent Events](http://dev.w3.org/html5/eventsource/#server-sent-events-intro) protocol for that purpose.

##### Responses
* **200 OK** with ``content-type: text/event-stream`` and the body of that response will be a stream of events, with the following format for each event:
```http
event: [EVENT-NAME]
data: [EVENT-DATA]
```
In that structure:
  * ``EVENT-NAME`` will be a one of the valid [Events](#events)
  * ``EVENT-DATA`` will be a json structure like the ones you can see in the examples below

##### Events

The currently supported events are:

###### ``game_status``
The event data is a json representation of a game, like the result from `GET /games/:game_id`

###### ``serpent_added``
The event data is a json representation of a serpent, like the one described in the result from `GET /games/:game_id`

###### ``game_countdown``
The event data is a json representation of a game, like the result from `GET /games/:game_id`

###### ``game_started``
The event data is a json representation of a game, like the result from `GET /games/:game_id`

###### ``game_updated``
The event data is a json representation of a game, like the result from `GET /games/:game_id`

###### ``collision_detected``
The event data is a json representation of a serpent, like the one described in the result from `GET /games/:game_id`

###### ``game_finished``
The event data is a json representation of a game, like the result from `GET /games/:game_id`
