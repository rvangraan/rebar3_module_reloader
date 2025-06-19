
-module(rebar3_module_reloader_app).

-behaviour(application).
-behaviour(supervisor).
%%-----------------------------------------------------------------------------------------------------------
-export([
    start/2,
    stop/1
]).
%%-----------------------------------------------------------------------------------------------------------
-export([
    start_link/1,
    init/1
]).
%%-----------------------------------------------------------------------------------------------------------

start(_Type, StartArgs) ->
    ?MODULE:start_link(StartArgs).

stop(_State) ->
    ok.

%%-----------------------------------------------------------------------------------------------------------

start_link([]) ->
    supervisor:start_link(?MODULE, []).

%%-----------------------------------------------------------------------------------------------------------

init([]) ->
    {ok,
        {{one_for_all, 2, 5}, [
             reloader_events_spec(),
             reloader_registry_spec()
        ]}}.

reloader_events_spec() ->
    #{
        id => rebar3_module_reloader_events,
        start => {rebar3_module_reloader_events, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [rebar3_module_reloader_events]
    }.

reloader_registry_spec() ->
    #{
        id => rebar3_module_reloader_registry,
        start => {rebar3_module_reloader_registry, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [rebar3_module_reloader_registry]
    }.