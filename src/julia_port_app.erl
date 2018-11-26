-module(julia_port_app).
-behaviour(application).

-include("julia_port.hrl").

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _Pid} = julia_port_sup:start_link().

stop(_State) ->
    ok.

