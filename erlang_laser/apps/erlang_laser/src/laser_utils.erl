-module(laser_utils).
-export([
    find/2
]).

find(Fn, List) when is_list(List), is_function(Fn, 1) ->
    case lists:dropwhile(fun (E) -> not Fn(E) end, List) of
        [] -> undefined;
        [E|_] -> E
    end.