-module(laser_lsp).

-export([
    server_caps/0
]).


server_caps() -> 
    #{
        hoverProvider => true
    }.