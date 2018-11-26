-module(julia_port_handler).

-include("julia_port.hrl").

-export([is_loaded/0,
         get_state/0]).

-export([start_link/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {
          driver_state :: {loaded, pid()} |
                          {reduced_mode, Error :: any()}
         }).

-define(TIMEOUT, 5000).

%%==============================================================================
%% INITIALIZATION
%%==============================================================================

start_link([]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = load_shared_lib(),
    {ok, #state{driver_state = State}}.

%%==============================================================================
%% PUBLIC
%%==============================================================================

is_loaded() ->
    gen_server:call(?MODULE, is_loaded, ?TIMEOUT).

get_state() ->
    gen_server:call(?MODULE, get_state, ?TIMEOUT).

%%==============================================================================
%% CALLBACKS
%%==============================================================================

handle_call(is_loaded, _From, #state{driver_state = DriverState} = State) ->
    case DriverState of
        {loaded, _Pid} ->
            {reply, true, State};
        {reduced_mode, _Error} ->
            {reply, false, State}
    end;
handle_call(get_state, _From, #state{driver_state = DriverState} = State) ->
    {reply, DriverState, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

load_shared_lib() ->
    try
        check_async_thread_pool_size(),
        SoPath = get_julia_lib_path(),
        {ok, Pid} = start_port_driver(SoPath),
        init_port_driver(Pid),
        {loaded, Pid}
    catch
        error:{julia_port_startup_error, _ErrorAtom, _ErrorArgs} = Error ->
            {reduced_mode, Error}
    end.

check_async_thread_pool_size() ->
    case erlang:system_info(thread_pool_size) of
        Size when is_integer(Size) andalso Size > 0 ->
            ok;
        Other ->
            error({julia_port_startup_error,
                   inappropriate_async_thread_pool_size,
                   [{thread_pool_size, Other}]})
    end.

start_port_driver(JuliaLibPath) ->
    os:putenv("JULIA_PORT_LIBJULIA_SO", JuliaLibPath),
    case gen_driver:start_link(code:priv_dir(?APPLICATION), ?PORT_DRIVER) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Error} ->
            error({julia_port_startup_error,
                   loading_port_driver_failed,
                   [{port_driver_path, [code:priv_dir(?APPLICATION), ?PORT_DRIVER]},
                    {julia_lib_path, JuliaLibPath},
                    {error, erl_ddll:format_error(Error)}]})
    end.

init_port_driver(Pid) ->
    try gen_server:call(Pid, {port, ?JULIA_PORT_INIT_CMD}, ?TIMEOUT) of
        ok ->
            ok;
        {error, Error} ->
            error({julia_port_startup_error,
                   port_driver_initialization_failed,
                   [{driver_error, Error}]})
    catch
        Error:Reason ->
            error({julia_port_startup_error,
                   port_driver_initialization_failed,
                   [{exception, Error}, {reason, Reason}]})
    end.

get_julia_lib_path() ->
    case do_get_julia_lib_path() of
        undefined ->
            error({julia_port_startup_error, julia_lib_path_is_undefined, []});
        SoPath ->
            SoPath
    end.

do_get_julia_lib_path() ->
    case application:get_env(?APPLICATION, julia_lib_path) of
        undefined ->
            case os:getenv("JULIA_PORT_LIBJULIA_SO") of
                false ->
                    undefined;
                SoPath ->
                    SoPath
            end;
        {ok, SoPath} ->
            SoPath
    end.
