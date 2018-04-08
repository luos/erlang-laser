-module(laser_workspace).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(ConnectionPid) ->
    lager:info("Start link workspace ~p", [self()]),
     {ok, Pid} = supervisor:start_link(?MODULE, [ConnectionPid]),
     {ok, Pid}.

init(ConnectionPid) -> 
    {ok, { {one_for_one, 0, 1}, [
        #{ id => laser_workspace_msg_handler, 
           start => {laser_workspace_msg_handler, start_link, [ConnectionPid]},
           restart => temporary
         },
         #{ id => laser_workspace_files, 
           start => {laser_workspace_files, start_link, [self()]},
           restart => temporary
         }
    ]} }.

