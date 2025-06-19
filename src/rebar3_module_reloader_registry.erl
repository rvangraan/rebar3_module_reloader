-module(rebar3_module_reloader_registry).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, add_erlang_source_file/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {ets_tab}).
-record(reloader_file_entry, {key, module, filename, header,header_reference}).

-define(ETS, rebar3_module_reloader_events).



%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% 
add_erlang_source_file(ErlangSourceFile) when is_binary(ErlangSourceFile) ->
    {ok, IncludeFileReferences} = read_and_parse_file(ErlangSourceFile),
    Entries = make_entries(ErlangSourceFile, IncludeFileReferences),
    [ ets:insert(?ETS, Entry) || Entry <- Entries ],
    ok.
%%--------------------------------------------------------------------
init([]) ->
    Tab = ets:new(?ETS, [named_table, public, set, {keypos, #reloader_file_entry.key}]),
    {ok, #state{ets_tab = Tab}}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% 
read_and_parse_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Binary} ->
            IncludeFileReferences = extract_include_files(Binary),
            {ok, IncludeFileReferences};
        {error, Reason} ->
            {error, Reason}
    end.
%%--------------------------------------------------------------------

extract_include_files(Binary) ->
    Lines = string:split(binary_to_list(Binary), "\n", all),
    Includes = lists:foldl(fun(Line, Acc) ->
        case re:run(Line, "-include(?:_lib)?\\([\"<]([^\">]+)[\">]\\)", [{capture, [1], list}]) of
            {match, [Include]} ->
                [coerce_binary(Include) | Acc];
            nomatch ->
                Acc
        end
    end, [], Lines),
    lists:reverse(Includes).

extract_include_files_test_() ->
    [
    ?_assertMatch(
        [<<"foo.hrl">>, <<"../lib/bar.hrl">>],
        extract_include_files(<<"-include(\"foo.hrl\").\n-include_lib(\"../lib/bar.hrl\").">>)
    ),
    ?_assertMatch(
        [],
        extract_include_files(<<"-module(foo).\n-export([bar/0]).">>)
    )].

%%--------------------------------------------------------------------

make_entries(ErlangSourceFile, IncludeFileReferences) ->
    SourceFile = coerce_binary(ErlangSourceFile),
    ModuleName = filename:basename(SourceFile, <<".erl">>),
    Module = binary_to_atom(ModuleName),

    Entries = [ make_entry(Module, SourceFile, IncludeFileReference)
              || IncludeFileReference <- IncludeFileReferences ],
    Entries.
%%--------------------------------------------------------------------
make_entries_test_() ->
    [
    ?_assertMatch(
        [#reloader_file_entry{key = {foo, <<"bar.hrl">>}, module=foo, filename = <<"/abc/src/foo.erl">>, header_reference = <<"bar.hrl">>}],
        make_entries("/abc/src/foo.erl", [<<"bar.hrl">>])
    ),
    ?_assertMatch(
        [#reloader_file_entry{key = {foo, <<"bar.hrl">>}, module=foo, filename = <<"foo.erl">>, header_reference = <<"abc/lib/bar.hrl">>}],
        make_entries(<<"foo.erl">>, [<<"abc/lib/bar.hrl">>])
    )].
%%--------------------------------------------------------------------

make_entry(ErlangModule, SourceFile, IncludeFileReference) ->
    Key = make_key(ErlangModule, IncludeFileReference),
    IncludeFile = normalise_include_file_reference(IncludeFileReference),
    #reloader_file_entry{key = Key, 
                         module=ErlangModule, 
                         filename = coerce_binary(SourceFile), 
                         header = IncludeFile,
                         header_reference=coerce_binary(IncludeFileReference)}.
%%--------------------------------------------------------------------

make_key(Module, IncludeFileReference) when is_atom(Module) ->
    IncludeFile = normalise_include_file_reference(IncludeFileReference),
    _Key = {Module, IncludeFile}.

%%--------------------------------------------------------------------
normalise_include_file_reference(IncludeFileReference) when is_list(IncludeFileReference) orelse
                                                            is_binary(IncludeFileReference) ->
    filename:basename(coerce_binary(IncludeFileReference)).
%%--------------------------------------------------------------------

normalise_include_file_reference_test_() ->
    [
    ?_assertMatch(
        <<"foo.hrl">>,
        normalise_include_file_reference("foo.hrl")
    ),
    ?_assertMatch(
        <<"foo.hrl">>,
        normalise_include_file_reference(<<"abc/lib/foo.hrl">>)
    )].

%%--------------------------------------------------------------------

coerce_binary(Binary) when is_binary(Binary) ->
    Binary;
coerce_binary(List) when is_list(List) ->
    list_to_binary(List).
%%--------------------------------------------------------------------


