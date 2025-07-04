%% @doc
%% Add the plugin to your rebar config, since it is a developer tool and not
%% necessary for building any project you work on I put it in
%% `~/config/.rebar3/rebar.config`:
%%
%% ```
%% {plugins, [rebar3_auto]}.'''
%%
%% Then just call your plugin directly in an existing application:
%%
%% ```
%% $ rebar3 auto
%% ===> Fetching rebar_auto_plugin
%% ===> Compiling rebar_auto_plugin'''
%%
-module(rebar3_module_reloader).
-behaviour(provider).

-export([init/1
        ,do/1
        ,format_error/1]).

-export([auto/1, flush/0]).

-define(PROVIDER, auto).
-define(DEPS, [compile]).


%%% TODO: gen_event to distribute change events
%%% TODO: make the auto call robust for exceptions
%%% TODO: compile "new" modules to ../ebin if it exists
%%% TODO: detect include directories and add them to the compile options
%%% TODO: locking mechanism to prevent multiple recompiles

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},        % The 'user friendly' name of the task
            {module, ?MODULE},        % The module implementation of the task
            {bare, true},             % The task can be run by the user, always true
            {deps, ?DEPS},            % The list of dependencies
            {example, "rebar3 auto"}, % How to use the plugin
            {opts, [{config, undefined, "config", string,
                     "Path to the config file to use. Defaults to "
                     "{shell, [{config, File}]} and then the relx "
                     "sys.config file if not specified."},
                    {name, undefined, "name", atom,
                     "Gives a long name to the node."},
                    {sname, undefined, "sname", atom,
                     "Gives a short name to the node."},
                    {setcookie, undefined, "setcookie", atom,
                     "Sets the cookie if the node is distributed."},
                    {script_file, undefined, "script", string,
                     "Path to an escript file to run before "
                     "starting the project apps. Defaults to "
                     "rebar.config {shell, [{script_file, File}]} "
                     "if not specified."},
                    {apps, undefined, "apps", string,
                     "A list of apps to boot before starting the "
                     "shell. (E.g. --apps app1,app2,app3) Defaults "
                     "to rebar.config {shell, [{apps, Apps}]} or "
                     "relx apps if not specified."}]},
            {short_desc, "Automatically run compile task on change of source file and reload modules."},
            {desc, ""}
    ]),
    application:ensure_all_started(rebar3_module_reloader),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Opts = rebar_state:get(State, auto, []),
    proc_lib:spawn(fun() ->
            listen_on_project_apps(State, Opts),
            Extensions = get_extensions(State, Opts),
            ?MODULE:auto(Extensions)
        end),
    io:format("~p started, listening for changes...~n", [?MODULE]),
    proc_lib:spawn(fun() -> list_source_files(State, "erl") end),
    State1 = remove_from_plugin_paths(State),
    rebar_prv_shell:do(State1).

-define(VALID_EXTENSIONS_DEFAULT,[<<".erl">>, <<".hrl">>]).

get_extensions(State, Opts) ->
    ExtraExtensions1 = rebar_state:get(State, extra_extensions, []), %%left for backward compatibility
    ExtraExtensions2 = proplists:get_value(extra_extensions, Opts, []),
    ExtraExtensions = ExtraExtensions1 ++ ExtraExtensions2,
    [unicode:characters_to_binary(Ext) || Ext <- ExtraExtensions] ++ ?VALID_EXTENSIONS_DEFAULT.

auto(Extensions) ->
    case whereis(rebar_agent) of
        undefined ->
            timer:sleep(100);

        _ ->
            receive
		{_Pid, _Type, {ChangedFile, _Events}} ->
                    Ext = filename:extension(unicode:characters_to_binary(ChangedFile)),
                    IsValid = lists:any(
                        fun(ValidExt) ->
                            RE = <<ValidExt/binary, "$">>,
                            Result = re:run(Ext, RE),
                            case Result of
                                {match, _Captured} -> true;
                                match -> true;
                                nomatch -> false;
                                {error, _ErrType} -> false
                            end
                        end,
                        Extensions
                    ),
                    case IsValid of
                        false -> pass;
                        true ->
                            %% split the file name and extension                           
                            BaseName = filename:basename(ChangedFile),
                            Extension = filename:extension(BaseName),
                            case Extension of
                                ".erl" -> 
                                    recompile_erl_file(ChangedFile);
                                ".hrl" ->
                                    io:format("Header file changed: ~s~n", [ChangedFile]),
                                    DependentModules = rebar3_module_reloader_registry:get_source_dependencies(ChangedFile),
                                    lists:foreach(
                                        fun recompile_erl_file/1,
                                        DependentModules
                                    ),
                                    ok;
                                            
                                _ ->
                                    ok
                            end,
                            ok
                    end;
                _ -> pass
            end

    end,
    ?MODULE:auto(Extensions).


recompile_erl_file(ErlFileName) ->
    PossibleModuleName = filename:basename(ErlFileName, ".erl"),
    PossibleModule = case PossibleModuleName  of
                        B when is_binary(B) -> 
                            list_to_atom(binary_to_list(PossibleModuleName));
                        L when is_list(L) ->
                            list_to_atom(PossibleModuleName)
                    end,
    case code:is_loaded(PossibleModule) of
        {file, _} ->
            R = c:c(PossibleModule, [brief,report_errors]),
            case R of
                {ok, _} -> 
                    ok;
                {error, _} -> 
                    ok;
                error ->
                    ok
            end,
            ok;
        _ when is_binary(ErlFileName)-> 
            io:format("Module ~s not loaded, performing full compile~n", [PossibleModuleName]),
            R = c:c(binary_to_list(ErlFileName)),
            io:format("Result ~p~n", [R]),
            R;
        _ when is_list(ErlFileName)-> 
            io:format("Module ~s not loaded, performing full compile~n", [PossibleModuleName]),
            R = c:c(ErlFileName),
            io:format("Result ~p~n", [R]),
            R

    end.


flush() ->
    receive
        _ ->
            flush()
    after
        0 -> ok
    end.

listen_on_project_apps(State, Opts) ->
    CheckoutDeps = [AppInfo ||
        AppInfo <-rebar_state:all_deps(State),
        rebar_app_info:is_checkout(AppInfo) == true
    ],
    ExtraDirs = [unicode:characters_to_binary(D) || D <-  proplists:get_value(extra_dirs, Opts, [])],
    ProjectApps = rebar_state:project_apps(State),
    lists:foreach(
        fun(AppInfo) ->
            AppDir = rebar_app_info:dir(AppInfo),
            Dirs = ExtraDirs ++ [<<"src">>, <<"include">>],
            lists:foreach(
                fun(Dir) ->
                    Path = filename:join(AppDir, Dir),
                    case filelib:is_dir(Path) of
                        true ->
                            Handle = binary_to_atom(<<"fs_watcher_", Path/binary>>),
                            fs:start_link(Handle, Path),
                            fs:subscribe(Handle);
                        false -> ignore
                    end
                end,
                Dirs
            )
        end,
        ProjectApps ++ CheckoutDeps
    ).

remove_from_plugin_paths(State) ->
    PluginPaths = rebar_state:code_paths(State, all_plugin_deps),
    PluginsMinusAuto = lists:filter(
        fun(Path) ->
            Name = filename:basename(Path, "/ebin"),
            not (list_to_atom(Name) =:= rebar_auto_plugin
                orelse list_to_atom(Name) =:= fs)
        end, 
        PluginPaths
    ),
    rebar_state:code_paths(State, all_plugin_deps, PluginsMinusAuto).


%% List all .erl files in the project by scanning each app's "src" directory.
-spec list_source_files(rebar_state:t(), list()) -> [binary()].
list_source_files(State, Extension) ->
    %% Retrieve project applications and checked-out dependencies
    ProjectApps = rebar_state:project_apps(State),
    CheckoutDeps = [AppInfo || AppInfo <- rebar_state:all_deps(State),
                              rebar_app_info:is_checkout(AppInfo) =:= true],
    AllApps = ProjectApps ++ CheckoutDeps,


    lists:foreach(
      fun(AppInfo) ->
              AppDir = rebar_app_info:dir(AppInfo),
              proc_lib:spawn(fun() ->
                  process_app_source_files(AppDir, Extension)
              end)
              
      end,
      AllApps),
    ok.


process_app_source_files(AppDir, Extension) ->
    SrcDir = filename:join(AppDir, <<"src">>),
    RegExp = ".*\." ++ Extension++"$",
    case filelib:is_dir(SrcDir) of
        true ->
            Files = filelib:fold_files(
                        SrcDir,
                        RegExp,
                        true,
                        fun(File, L) -> [File | L] end,
                        []),
            [ rebar3_module_reloader_registry:add_erlang_source_file(File) || File <- Files ],
            normal;
        false ->
            normal
    end.