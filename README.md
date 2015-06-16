# Serpents
Multi-Player Game on top of HDP protocol or REST+SSE

## Introduction
Serpents is the classic snakes game where each player controls a serpent that moves around a board. When the serpent eats a fruit it grows according to how many food the fruit provides. If the serpent hits anything but a fruit (being that a wall, another serpent or even its own body) the serpent dies.

## The Server
This repo provides the game server. Players should write their clients on their own. To do that they can use one of two protocols:

- [HTTP](HTTP.md): A RESTful API with an additional SSE endpoint for updates
- [HDP](HDP.md): An UDP based protocol

To start the server you can run
```bash
$ make && make shell
```

Or you can generate a release with `make` and then start it
```bash
$ make && _rel/serpents/bin/serpents start
```

## The Website
Once the server is started you can open [http://localhost:8585](http://localhost:8585) and start creating and watching games.
