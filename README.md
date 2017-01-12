battlecraft
=====

battlecraft is a simple multiplayer browser-based game built with erlang. It is an implementation of a distributed game engine. The objective of the game is to spawn units to destroy your opponents base.

## Install

### Requirements
* erlang otp 18+
* node + npm

### Instructions

```
$ git clone https://github.com/jbreindel/battlecraft
$ cd battlecraft
$ rebar3 release
$ _build/default/rel/bc/bin/bc start
```

## Developing

battlecarft is built using [rebar3](http://www.rebar3.org/), and [gulp](http://gulpjs.com/). I also highly recommend using [observer](http://erlang.org/doc/apps/observer/observer_ug.html) to inspect erlang at runtime. Any rebar3 commands and usage can also be used to debug, and open shells to battlecarft.

## Motivations

I wrote battlecraft to understand how to build a complete game and game engine. I wanted to divorce the idea from the traditional game loop and model the AI entities in an asynchronous system.