-module(laser_server).

-export([start/1]).
-export([stop/0]).

start(Port) ->
	laser_tcp_server:start(10, Port).

stop() ->
	ok.