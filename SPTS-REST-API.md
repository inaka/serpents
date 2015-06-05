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
