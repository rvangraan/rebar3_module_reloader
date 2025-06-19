-module(rebar3_module_reloader_events).

-export([start_link/0,
         add_sup_handler/2]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_sup_handler(Module, Args) ->
    gen_event:add_sup_handler(?MODULE, Module, Args).
