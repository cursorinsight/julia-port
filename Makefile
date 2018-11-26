# Build tools
REBAR := $(shell which rebar3)
JULIA := julia
CC := gcc

# Common directories and paths
TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
ABS_DIR := $(abspath $(TOP_DIR))
BUILD_DIR := $(TOP_DIR)/_build

# Julia HOME
JULIA_HOME ?= $(shell julia -e 'print(Base.JULIA_HOME)')

# Specific Erlang flags that is compatible with this project
BEAM_FLAGS := ERL_FLAGS="$(ERL_FLAGS) +A4 +a512"

.PHONY: all deps test

# Default targets
all: compile
everything: mrproper compile test

# Check for missing build tools
ifeq "$(strip $(REBAR))" ""
REBAR := rebar
endif

$(REBAR):
	@echo Please install \`$@\' manually!
	@exit 1

#------------------------------------------------------------------------------
# Julia specific targets
#------------------------------------------------------------------------------

pre-test: ${TOP_DIR}/test/julia_port_SUITE_data/libjulia-dev.so

${TOP_DIR}/test/julia_port_SUITE_data/libjulia-dev.so:
	cd ${TOP_DIR}/test/julia_port_SUITE_data && \
	${JULIA} ${ABS_DIR}/priv/build_sysimg.jl ${ABS_DIR}/test/julia_port_SUITE_data/libjulia-dev x86-64 userimg.jl --force && \
	rm -f libjulia-dev.so && \
	${CC} -shared -ljulia -o libjulia-dev.so -Wl,--whole-archive libjulia-dev.o -Wl,--no-whole-archive && \
	cd ${ABS_DIR}

#------------------------------------------------------------------------------
# Targets
#------------------------------------------------------------------------------

mrproper:
	rm -rf $(BUILD_DIR)

clean_c_binaries:
	rm -rf priv/julia_port.so
	rm -f c_src/*.o

clean_libjulia:
	rm -f test/julia_port_SUITE_data/*.o
	rm -f test/julia_port_SUITE_data/*.so
	rm -f test/julia_port_SUITE_data/*.ji

clean_all: mrproper clean_c_binaries clean_libjulia

deps: $(REBAR)
	$(REBAR) upgrade

compile: $(REBAR)
	$(REBAR) compile

test: compile pre-test
	$(BEAM_FLAGS) $(REBAR) do dialyzer, eunit, ct

shell:
	$(BEAM_FLAGS) $(REBAR) shell
