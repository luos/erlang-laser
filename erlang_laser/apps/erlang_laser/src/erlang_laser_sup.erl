%%%-------------------------------------------------------------------
%% @doc erlang_laser top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_laser_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).



-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 0, 1}, [
        #{ id => laser_server, 
           start => {laser_tcp_server, start_link, [
               9999,
               fun laser_workspaces_sup:create/1
            ]},
           restart => permanent
         },
         #{ 
            id => laser_workspaces_sup,
            start => {laser_workspaces_sup, start_link, []},
            restart => permanent
            }
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
