-module(julia_port).

-include("julia_port.hrl").

-export([call/4]).

-spec call(jl_module(), jl_function(), jl_args(), timeout()) -> {ok, jl_response()} |
                                                                {error, term()}.
call(JlModule, JlFunction, Args, Timeout) when is_binary(Args) ->
    case julia_port_handler:get_state() of
        {loaded, Pid} ->
            gen_server:call(Pid, {port, ?JULIA_PORT_CMD, {JlModule, JlFunction, Args}}, Timeout);
        {reduced_mode, Error} ->
            error({julia_port_in_reduced_mode, Error})
    end.
