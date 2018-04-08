-module(laser_src_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("laser_workspace.hrl").

given_not_existing_file_returns_file_not_found_test() -> 
  ?assertEqual({error, file_not_found}, 
    parse("/tmp/whatever")).

given_an_empty_file_returns_empty_file_error_test() -> 
    ?assertEqual({error, file_empty}, 
        parse("apps/erlang_laser/test_resources/src_parser/empty_file.erl")).

given_an_empty_module_reads_module_name_test() -> 
  #erl_file_info{
    module_name = ModuleName
  } = parse_resource("empty_module.erl"),

  ?assertEqual(empty_module,ModuleName).

given_a_simple_module_finds_exports_test() -> 
  #erl_file_info{
    module_name = ModuleName,
    export_funs = Exports
  } = parse_resource("simple_module.erl"),
  ?assertEqual(simple_module,ModuleName),
  ?assertEqual([{fn, 1}, {fn2, 2}],Exports).

given_a_simple_module_finds_defined_functions_test() -> 
    #erl_file_info{
      defined_funs = DefinedFuns
    } = parse_resource("simple_module.erl"),
    ?assertEqual(4, length(DefinedFuns)),
    assert_fun_defined({fn, 1, 13}, DefinedFuns),
    assert_fun_defined({fn2, 2, 15}, DefinedFuns),
    assert_fun_defined({not_exported, 1, 17}, DefinedFuns),
    assert_fun_defined({not_exported2, 2, 19}, DefinedFuns).

given_multiple_claues_for_fun_parses_donno_yet_test() -> 
    #erl_file_info{
      defined_funs = DefinedFuns
    } = parse_resource("multi_clause_fn.erl"),
    assert_fun_defined({fn, 1, 7}, DefinedFuns),
    ?assertEqual(1, length(DefinedFuns)),
    [{fn, _, _, _Clauses = [
      First, Second, Third, Fourth, Fifth, _ , _
    ]}] = DefinedFuns,
    ?assertEqual({[{val, atom, first_arg}], 7}, First),
    ?assertEqual({[{val, atom, second_arg}], 8}, Second),
    ?assertEqual({[{val, integer, 3}], 9}, Third),
    ?assertEqual({[{val, list, []}], 10}, Fourth),
    ?assertEqual({[{val, list, [
      {val, integer, 1},
      {val, integer, 2},
      {val, integer, 3}
    ]}], 11}, Fifth).

    


assert_fun_defined({FnName, Arity, Line} = ExpectedFn, DefinedFuns) -> 
  case laser_utils:find(fun(Fn) ->
      case Fn of 
        {FnName, Arity, Line, _} -> true;
        _ -> false
      end
    end, DefinedFuns) of 
    Value when not is_atom(Value) -> 
      ok;
    false -> 
      ?debugFmt("Couldn't find fn ~p in ~p",[ExpectedFn, DefinedFuns]),
      ?assert(false)
  end.

parse(Path) -> 
  laser_src_parser:parse_file(Path).

parse_resource(Path) -> 
    P = "apps/erlang_laser/test_resources/src_parser/" ++ Path,
    parse(P).