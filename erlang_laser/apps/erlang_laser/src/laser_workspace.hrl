
-record(ws_pids, {
  msg_handler,
  ws_files
}).

-type ws_pids() :: #ws_pids{}.

-record(erl_file_info, {
    file_path :: string(),
    syntax_form :: erl_syntax:syntaxTree(),
    binary_content :: binary(),
    module_name :: atom(),
    export_funs = [] :: [term()],
    defined_funs = [] :: [defined_fun()]
}).

-type defined_fun() :: {
  FnName :: atom(),
  Arity :: pos_integer(), 
  Line :: pos_integer() 
}.

-type erl_file_info() :: #erl_file_info{}.

