-module(laser_src_parser).
-include("laser_workspace.hrl").

-export([
    parse_file/1
]).


% -spec parse_file(Path :: string(), term()) -> 
%     {ok, Result :: term()} | {error, Errors :: term()}.
parse_file(Path) ->
    case file:read_file(Path) of 
        {ok, ContentBin} -> 
            parse_bin(Path, ContentBin);
        {error, enoent} -> {error, file_not_found};
        {error, _} = Error -> Error
    end.

% TODO if file is not really empty but only whitespace
parse_bin(_Path, <<>>) -> 
    {error, file_empty};
parse_bin(Path, _Content) -> 
    {ok, Forms} = epp_dodger:parse_file(Path),
    io:format("Forms ~p ~n",[Forms]),
    #erl_file_info{
        module_name = get_module_name(Forms),
        export_funs = get_export_funs(Forms),
        defined_funs = get_defined_funs(Forms)
    }.


get_module_name(Forms) -> 
    case laser_utils:find(is_attribute(module), Forms) of
        AttrForm -> 
            case attribute_val(AttrForm) of 
                [{tree, atom, _, ModuleName}] ->  ModuleName;
                _ -> undefined
            end
    end.

get_export_funs(Forms) -> 
    case laser_utils:find(is_attribute(exports), Forms) of
        undefined -> [];
        ExportForm -> 
            case attribute_val(ExportForm) of 
                [{tree, list, _Line, {list, FnForms, _Donno}}] -> 
                    io:format("Export ofrm ~p",[FnForms]),
                extract_funs(FnForms, [])
            end
    end.

extract_funs([Fun | Forms], FunDefs) -> 
  Extracted = case Fun of 
    {tree, tuple, _Line, [
        {tree, atom, _, FunName},
        {tree, integer, _, Arity}
        ]} -> {FunName, Arity}
  end,
  extract_funs(Forms, [Extracted | FunDefs]);

extract_funs([],FunDefs) -> lists:reverse(FunDefs).

-spec get_defined_funs(term()) -> [defined_fun()].
get_defined_funs(Forms) -> 
    Funs = lists:filter(fun is_function/1, Forms),
    lists:map(fun fun_def_val/1, Funs).

is_attribute(AttrName) ->
    fun({tree, attribute, _None, {attribute, Def, _Val}}) -> 
        case Def of 
            {tree,atom, _Line, AttrName} ->
                true;
            _ -> false
        end;
        (_) -> false
    end.

attribute_val({tree, attribute, _Line, {attribute, _Def, Val}}) ->  Val.

fun_def_val(
    {tree, function, 
    {attr, Line, _, _}, 
    {func, 
        {tree, atom, _, FnName}, 
        ClausesForm}})
    ->
        io:format("~nClause form: ~p~n", [ClausesForm]),
        Clauses = extract_fun_clauses(ClausesForm, []),
    {FirstArgs, _} = lists:nth(1, Clauses),
    FormattedClauses = format_clauses(Clauses,[]),
    Arity = length(FirstArgs),
    {FnName, Arity, Line, FormattedClauses}.

extract_fun_clauses([Clause | Rest], ClauseInfos) -> 
    {tree, clause, {attr, Line, _, _}, {clause, FnArgs, none, _FnClause}} = Clause,
    extract_fun_clauses(Rest, [{FnArgs, Line} | ClauseInfos]);

extract_fun_clauses([], ClauseInfos) -> lists:reverse(ClauseInfos).

fun_arg_to_useful({atom, _, Value}) -> 
    {val, atom, Value};
fun_arg_to_useful({integer, _, Value}) -> 
    {val, integer, Value};
fun_arg_to_useful({nil, _}) ->
    {val, list, []};
fun_arg_to_useful({tree, list, _, {list, Values, _}}) -> 
    {val, list, lists:map(fun fun_arg_to_useful/1, Values)};
fun_arg_to_useful({var, _, VarName}) -> 
    {var, VarName};
fun_arg_to_useful(Value) ->
    {val, unknown, Value}.
            
format_clauses([{Args, Line}| Rest], Formatted) -> 
    UsefulArgs = lists:map(fun fun_arg_to_useful/1, Args),
    format_clauses(Rest, [{UsefulArgs, Line} | Formatted]);
format_clauses([], Formatted) -> lists:reverse(Formatted).

is_function({tree, function, _Line, _FnForm}) -> true;
is_function(_) -> false.

