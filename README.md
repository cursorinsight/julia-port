julia_port
==========

This project is an Erlang port driver solution for Erlang - Julia communication.

Prerequisites
-------------

* Julia v1.0.0 installation is required.
* Erlang 19 is required. (The build process doesn't work on Erlang/OTP 21.)

Configuration
-------------

You need to make sure that the correct `libjulia.so` is found when julia_port
builds its SO file. If you have Julia installed properly and
`/usr/lib/libjulia.so` belongs to your preferred Julia installation, then
nothing needs to be done.

Otherwise you can select the appropriate Julia installation by adding it to the
`LD_LIBRARY_PATH` environment variable. The following setting is needed not only
for the build but also for the execution, so it might be a good idea to put it
in `~/.bashrc`. `JULIA_INSTALLATION_DIR` should point to the Julia installation
that you wish to use.

```sh
export LIBRARY_PATH=$JULIA_INSTALLATION_DIR/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$JULIA_INSTALLATION_DIR/lib:$LD_LIBRARY_PATH
```

Test
----

Create `libjulia-dev.so` that will be used by the automatic tests. This SO file
will contain the Julia Base and the Julia functions implemented in
`test/julia_port_SUITE_data/userimg.jl`.
See more information at [Julia's System Image
Building documentation page](https://docs.julialang.org/en/stable/devdocs/sysimg/).

See the related issues [1](https://github.com/JuliaLang/julia/issues/28557),
[2](https://github.com/JuliaLang/julia/issues/27451) for Julia 1.0.0.

Build the SO file with `make pre-test`:

```sh
$ make pre-test
```

You can use `ldd` to check that the correct `libjulia.so` file is found when
reading `libjulia-dev.so`:

```sh
$ ldd test/julia_port_SUITE_data/libjulia-dev.so
        ...
        libjulia.so.1 => /usr/lib/libjulia.so.1 (0x00007fa9c344d000)
        ...
```

Run tests:

```sh
$ make test
```

Shell
-----

* create libjulia-dev.so as described in the Test section
* set julia_lib_path environment variable in sys.config to the created *so*
  file's path
* start interactive shell as:

    ```sh
    $ rebar3 shell
    ```
