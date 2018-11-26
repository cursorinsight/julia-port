%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, CURSOR INSIGHT HUNGARY
%%% @doc
%%%
%%% @end
%%% Created : 21. Feb 2018 4:27 PM
%%%-------------------------------------------------------------------
-module(julia_port_test).

%% API
-export([call/3]).

-define(TIMEOUT, 30000).

call(JlModule, JlFunction, Args) ->
    case julia_port:call(JlModule, JlFunction, Args, ?TIMEOUT) of
        {ok, Response} ->
            Response;
        {error, Error} ->
            error(Error);
        Error ->
            error(Error)
    end.
