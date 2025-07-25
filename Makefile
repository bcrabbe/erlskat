# Makefile (erlang.mk) — fixed for modern Erlang.mk
PROJECT             = erlskat
PROJECT_DESCRIPTION = New project
PROJECT_VERSION     = 0.1.0
RELX_TAR            = 0

# Remove -Werror that Erlang.mk now injects by default
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

# ------------ Runtime, build-time and shell deps ------------
SHELL_DEPS = sync
LOCAL_DEPS = sasl

BUILD_DEPS = elvis_mk
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git

DEPS = \
	cowboy \
	jsx    \
	uuid   \
	meck

# Versions / sources for third-party deps
dep_cowboy_commit = 2.13.0
dep_jsx = hex 2.9.0
dep_uuid = hex 1.7.5 uuid_erl
dep_meck = hex 1.0.0

DEP_PLUGINS = elvis_mk cowboy

EUNIT_OPTS  = verbose

# Extra options for `make shell`
SHELL_OPTS = \
	-s $(PROJECT) \
	-config dev.config \
	-s sync \
	+pc unicode

include erlang.mk

# Convenience aggregate target
all:: elvis eunit ct
