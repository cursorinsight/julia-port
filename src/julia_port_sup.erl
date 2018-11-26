-module(julia_port_sup).
-include("julia_port.hrl").
-behaviour(supervisor).

%% Supervisor API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%% API implementation
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callback
init(_Config) ->
    Children = [#{id => julia_port_handler,
                  start => {julia_port_handler, start_link, []},
                  restart => permanent,
                  shutdown => 1000,
                  type => worker,
                  modules => [julia_port_handler]}],
    {ok, {{one_for_all, 2, 1}, Children}}.

