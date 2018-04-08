-module(laser_workspaces_sup).
-behaviour(supervisor).
-include("laser_workspace.hrl").

-export([
    create/1,
    stop/1,
    start_link/0,
    init/1
]).

start_link() ->
     {ok, Pid} = supervisor:start_link({local, ?MODULE}, 
          ?MODULE, []),
     {ok, Pid}.

init(_Args) -> 
    {ok, { {simple_one_for_one, 0, 1}, [
        #{ id => laser_workspace, 
           start => {laser_workspace, start_link, []},
           restart => temporary,
           type => supervisor
         }
    ]} }.

create(Connection) -> 
    lager:info("Creating workspace supervisor.. ~p", [self()]),
    {ok, WsSupPid} = supervisor:start_child(?MODULE, [Connection]),
    lager:info("Suppid ~p",[WsSupPid]),
    Children = supervisor:which_children(WsSupPid),
    lager:info("Children ~p",[Children]),
    {laser_workspace_msg_handler,MsgHandlerPid, _, _} = lists:keyfind(laser_workspace_msg_handler, 1, Children),
    {laser_workspace_files, FileHandlerPid, _, _} = lists:keyfind(laser_workspace_files, 1, Children),
    Pids = #ws_pids{
        msg_handler = MsgHandlerPid,
        ws_files = FileHandlerPid
    },
    ok = gen_server:call(MsgHandlerPid, {init, Pids}),
    {ok, MsgHandlerPid}.

stop(_ShutdownMsg) -> ok.