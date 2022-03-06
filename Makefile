PROJECT = erlskat
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
RELX_TAR = 0
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

SHELL_DEPS = \
	sync

LOCAL_DEPS = \
	sasl
BUILD_DEPS = \
       elvis_mk
DEPS = cowboy \
       jsx \
       uuid \
       meck
dep_jsx_commit = 2.9.0
dep_cowboy_commit = 2.9.0
DEP_PLUGINS = elvis_mk \
	      cowboy

EUNIT_OPTS = verbose
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git
SHELL_OPTS = \
	-s $(PROJECT) \
	-config dev.config \
	-s sync \
	+pc unicode

include erlang.mk

all:: elvis eunit ct
