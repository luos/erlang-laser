-module(simple_module).

-define(DEF_INT, 3).

-define(DEF_STR, "str").
-define(DEF_BIN, <<"bin">>).

-exports([
    fn/1,
    fn2/2
]).

fn(Args) -> not_exported(Args).

fn2(Args, Args2) -> not_exported2(Args, Args2).

not_exported(Args) -> Args.

not_exported2(Args, Args2) -> {Args, Args2}.