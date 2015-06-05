## RESTful API for Serpents

### Endpoints

#### ``POST /api/games/:game_id/serpents``
To create a serpent (i.e. join a game)

##### Parameters
All parameters are required
```json
{"name": [SERPENT-NAME]}
```

##### Responses
* **404 Not Found** if the game was not found
* **400 Bad Request** with a descriptive message
* **201 Created** with
```json
{ "name": [NAME]
, "token": [TOKEN]
, "body": [ [[HEAD-ROW#],[HEAD-COL#]]
          , [[ROW#],[COL#]]
          , [[ROW#],[COL#]]
          , …
          ]
, "direction": [DIRECTION]
, "status": [STATUS]
}
```
* `[CONTENT]` can be
  - `"fruit"`, in which case `[VALUE]` will be the amount of food it provides
  - `"wall"`, in which case `"value"` will not be there
* `[ROW#]` and `[COL#]` are 1-based
* `[DIRECTION]` can be "up", "down", "left" or "right"
* `[TOKEN]` is unique per serpent, **do not share it**

---

#### ``PUT /api/games/:game_id/serpents/:token``
To change the direction of the serpent

##### Parameters
All parameters are required
```json
{"direction": [DIRECTION]}
```

##### Responses
* **404 Not Found** if the game was not found
* **400 Bad Request** with a descriptive message
* **403 Forbidden** with the text "Forbidden"
* **200 OK** with the same json object as ``POST /games/:game_id/serpents``

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
The event data is a json representation of a serpent (see below)

###### ``game_countdown``
The event data is a json representation of a game (see below)

###### ``game_started``
The event data is a json representation of a game (see below)

###### ``game_updated``
The event data is a json representation of a game (see below)

###### ``collision_detected``
The event data is a json representation of a serpent (see below)

###### ``game_finished``
The event data is a json representation of a game (see below)

##### Objects
###### Game
```json
{ "id": [ID]
, "rows": [#-OF-ROWS]
, "cols": [#-OF-COLS]
, "ticktime": [#-OF-TICKTIME-MILLISECONDS]
, "countdown": [#-OF-COUNTDOWN-TICKS]
, "rounds": [MAX-#-OF-GAME-TICKS]
, "with": [LIST-OF-EXTRA-FLAGS]
, "initial_food": [INITIAL-FOOD-COUNT]
, "flags": [LIST-OF-FLAGS]
, "max_serpents": [MAX-#-OF-SERPENTS]
, "serpents": [{…}, {…}]
, "state": [STATE]
, "cells": [ {"row": [ROW#], "col": [COL#], "content": [CONTENT], "value": [VALUE]}
           , {"row": [ROW#], "col": [COL#], "content": [CONTENT]}
           , …
           ]
}
```
* `[STATE]` can be `"created"`, `"countdown"`, `"started"` or `"finished"`
* `[CONTENT]` can be
  - `"fruit"`, in which case `[VALUE]` will be the amount of food it provides
  - `"wall"`, in which case `"value"` will not be there
* `[ROW#]` and `[COL#]` are 1-based

###### Serpent
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
