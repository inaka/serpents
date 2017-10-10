# Serpents

<img src="https://s3.amazonaws.com/uploads.hipchat.com/15025/55900/VX1Hr6y6i3iVL7L/serpents.png" align="right" style="float:right" height="250px"/>

Multi-Player Game on top of HDP protocol or REST+SSE

### Introduction
**Serpents** is the classic snakes game where each player controls a serpent that moves around a board. When the serpent eats a fruit it grows according to how much food the fruit provides. If the serpent hits anything but a fruit (being that a wall, another serpent or even its own body) the serpent dies.

### The Server
This repo provides the game _server_. Players should write their clients on their own (and that is **part of the game**). To do that they can use one of two protocols:

- [HTTP](HTTP.md): A RESTful API with an additional SSE endpoint for updates
- [HDP](HDP.md): An UDP based protocol

To start the server, run
```bash
$ rebar3 compile && rebar3 shell
```

Or you can generate a release with `rebar3 release` and then start it
```bash
$ ./_build/default/rel/serpents/bin/serpents start
```

### The Website
Once the server is started you can open [http://localhost:8585](http://localhost:8585) and start creating and watching games.

### Player Examples
If you don't know where to start in order to write your serpents client, you can check the sample clients provided in the `src/clients` folder. There are 2 clients there:

* **ai**: An collection of automatic clients that connect to a game and automatically move the serpents according to their algorithms
* **wx**: An interactive client built on top of **wx**

---

## Contact Us
If you find any **bugs** or have a **problem** while using this project, please
[open an issue](https://github.com/inaka/shotgun/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at
[inaka.github.io](http://inaka.github.io)
