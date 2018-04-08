-module(laser_workspace_files).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([init_ws/2]).

init_ws(FielHandlerPid, Data) -> 
    gen_server:cast(FielHandlerPid, {init_ws, Data}).

start_link(_SupPid) ->
    gen_server:start_link(laser_workspace_files, [], []).

init(_Args) ->
    {ok, {}}.

handle_call(_Args, _From, State) ->
    {reply, ok, State}.

handle_cast(_Args, State) ->
    lager:info("Laser Files got cast: ~p", [{_Args, State}]),
    {noreply, State}.