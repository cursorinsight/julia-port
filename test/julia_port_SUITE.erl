% TODO: refactor app loading/starting/stopping/unloading parts (eg. things
% relating apps_before).

-module(julia_port_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     call_sqrt_valid,
     call_sqrt_invalid_module,
     call_sqrt_invalid_method,
     call_sqrt_bad_argument1,
     call_sqrt_bad_argument2
    ].

%%------------------------------------------------------------------------------
%% SUITE init/end
%%------------------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%------------------------------------------------------------------------------
%% TESTCASE init/end
%%------------------------------------------------------------------------------

init_per_testcase(common, Config) ->
    {ok, _} = application:ensure_all_started(julia_port),
    Config;

init_per_testcase(_TestCase, Config) ->
    set_env_variable(Config),
    init_per_testcase(common, Config).

end_per_testcase(_TestCase, _Config) ->
    ok = application:stop(julia_port).

call_sqrt_valid(_Config) ->
    ?assertEqual(<<"ok">>, julia_port_test:call(<<"JuliaPortTest">>, <<"check_function">>, <<"good">>)),
    ok.

call_sqrt_invalid_module(_Config) ->
    ?assertError(<<"ModuleError">>, julia_port_test:call(<<"JuliaPortTest2">>, <<"check_function">>, <<"good">>)),
    ok.

call_sqrt_invalid_method(_Config) ->
    ?assertError(<<"MethodError">>, julia_port_test:call(<<"JuliaPortTest">>, <<"non_existing_function">>, <<"good">>)),
    ok.

call_sqrt_bad_argument1(_Config) ->
    ?assertError(_, julia_port_test:call(<<"JuliaPortTest">>, <<"check_function">>, <<"bad">>)).

call_sqrt_bad_argument2(_Config) ->
    try
        julia_port_test:call(<<"JuliaPortTest">>, <<"check_function">>, <<"bad">>)
    catch
        error:{julia_port_in_reduced_mode, _, _} = Error ->
            erlang:raise(throw, Error);
        error:Error ->
            %% Check that the julia stacktrace is included
            ?assertMatch(<<"ERROR: DomainError\nStacktrace:\n [1] check_function(::Array{UInt8,1}) at",
                           _/binary>>,
                         Error)
    end.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

set_env_variable(Config) ->
    os:putenv("JULIA_PORT_LIBJULIA_SO", ?config(data_dir, Config) ++ "libjulia-dev.so").
