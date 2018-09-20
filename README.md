battlecraft
=====

battlecraft is a simple multiplayer browser-based game built with [erlang](http://www.erlang.org/) and [elm](http://elm-lang.org/). It is an implementation of a distributed game engine. The objective of the game is to spawn units to destroy your opponents base.

![battlecraft](/apps/bc_web/priv/static/img/battlecraft-cropped-800x450.png)

to begin a game simply create an appropriately sized game from the games page (/games) and share the game url with your friends.

### Controls 

Spawn units with the standard `wasd` keys and use the `arrow keys` to move the view.

## Install

### Requirements
* erlang otp 18+
* [rebar3](http://www.rebar3.org/docs/getting-started#section-installing-binary)
* node + npm

### Instructions

```
git clone https://github.com/jbreindel/battlecraft
cd battlecraft
rebar3 release
_build/default/rel/bc/bin/bc start
```

## Developing

battlecarft is built using [rebar3](http://www.rebar3.org/), and [gulp](http://gulpjs.com/). I also highly recommend using [observer](http://erlang.org/doc/apps/observer/observer_ug.html) to inspect erlang at runtime. Any rebar3 commands and usage can also be used to debug, and [open shells](http://www.rebar3.org/docs/commands#section-shell) to battlecarft.

## Motivations

I wrote battlecraft to understand how to build a complete game and game engine. I wanted to divorce battlecraft from the traditional game loop and model the AI entities in an asynchronous system.

## License

Licensed under the Apache License, Version 2.0: http://www.apache.org/licenses/LICENSE-2.0

## Thank You

Battlecraft would have not been possible without the assets from [opengameart.org](http://opengameart.org/) and the  [tiled map editor](http://www.mapeditor.org/).
