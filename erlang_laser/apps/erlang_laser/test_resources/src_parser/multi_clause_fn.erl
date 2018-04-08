-module(multi_clause_fn).

-export([
    fn/1
]).

fn(first_arg) -> first_arg_ret;
fn(second_arg) -> second_arg_ret;
fn(3) -> 5;
fn([]) -> ["list"];
fn([1,2,3]) -> ["long list"];
fn(<<"binary value">>) -> "string value";
fn(CatchAll) -> CatchAll.

