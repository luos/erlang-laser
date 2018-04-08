-module(laser_workspace_msg_handler).
-include("laser_workspace.hrl").
-behavior(gen_server).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  start_link/1
]).

-export([
  call/3
]).

-record(workspace_opts, {
  root_path :: string(),
  editor_caps :: term(),
  init_opts :: term()
}).

-type workspace_opts() :: #workspace_opts{}.

-record(state, {
  workspace_opts :: workspace_opts(),
  client :: term(),
  handlers :: term()
}).

-type state() :: #state{}.

start_link(Args) ->
  lager:info("Got args ~p", [Args]),
  gen_server:start_link(?MODULE, Args, []).

init(ClientPid) ->
  lager:info("Starting workspace ~p", [ClientPid]),
  {ok, #state{
    client = ClientPid
  }}.

handle_call({init, WsPids}, _From, State) -> 
  {reply, ok, State#state{
    handlers = WsPids
  }};


handle_call({method_call, {<<"initialize">>, Params}}, _From, State) ->
  _RootPath = proplists:get_value(<<"rootPath">>, Params),
  Reply = [{<<"capabilities">>, [
              {<<"hoverProvider">>, false},
              {<<"textDocumentSync">>, 1},
              {<<"definitionProvider">>, true},
              {<<"referencesProvider">>, true},
              {<<"referencesProvider">>, true},
              {<<"renameProvider">>, true}
          ]}],
  lager:info("Current state ~p ~n",[State]),
  laser_workspace_files:init_ws(file_handler(State), {"hello world"}),
  {reply, {ok, Reply}, State};

handle_call({method_call, {<<"initialized">>, _Params}}, _From, State) ->
  {reply, noreply, State};

handle_call({method_call, {<<"textDocument/didOpen">>, _Params}}, _From, State) ->
  {reply, noreply, State};

handle_call({method_call, {<<"textDocument/didSave">>, _Params}}, _From, State) ->
  {reply, noreply, State};
    

handle_call({method_call, {Message, Params}}, _From, State) ->
  lager:info("Got new message ~p", [{Message, Params}]),
  {reply, unknown_message, State};

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

handle_cast({send_error, {ErrorCode, Message}}, #state{
  client = Client
} = State) -> 
    laser_tcp_handler:send_error(Client, ErrorCode, Message),
    {noreply, State};

handle_cast(_Request, _State) ->
  lager:info("Got not implemented cast ~p",[{_Request, _State}]),
  erlang:error(not_implemented).

call(Workspace, Method, Params) ->
  gen_server:call(Workspace, {method_call, {Method, Params}}).


file_handler(#state{handlers = Handlers}) -> 
  Handlers#ws_pids.ws_files.
