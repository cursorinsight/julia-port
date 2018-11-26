% julia_port application's private header
-ifndef(JULIA_PORT_HRL).
-define(JULIA_PORT_HRL, true).

-export_type([jl_module/0,
              jl_function/0,
              jl_args/0,
              jl_response/0]).

-define(APPLICATION, julia_port).

-define(PORT_DRIVER, "julia_port").
-define(JULIA_PORT_INIT_CMD, 1).
-define(JULIA_PORT_CMD, 2).

-type jl_module() :: binary().
-type jl_function() :: binary().
-type jl_args() :: binary().
-type jl_response() :: binary().

-endif. % ifndef(JULIA_PORT_HRL)
