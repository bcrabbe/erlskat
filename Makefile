# Makefile (erlang.mk) — fixed for modern Erlang.mk
PROJECT             = erlskat
PROJECT_DESCRIPTION = Websocket-based multiplayer Skat card game server
PROJECT_VERSION     = 0.1.0
RELX_TAR            = 0

# Remove -Werror that Erlang.mk now injects by default
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

# ------------ Runtime, build-time and shell deps ------------
SHELL_DEPS += sync
dep_sync = hex 0.4.1
LOCAL_DEPS += sasl

# Release dependencies
REL_DEPS += relx

# Release configuration
RELX_CONFIG = $(CURDIR)/relx.config
RELX_OUTPUT_DIR = $(CURDIR)/_rel
RELX_OPTS = -d true  # Enable dev_mode for development

BUILD_DEPS += elvis_mk
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git

DEPS += cowboy
DEPS += jsx
DEPS += uuid
DEPS += meck

# Versions / sources for third-party deps
dep_cowboy_commit = 2.13.0
dep_jsx = hex 2.9.0
dep_uuid = hex 1.7.5 uuid_erl
dep_meck = hex 1.0.0

DEP_PLUGINS += elvis_mk

EUNIT_OPTS += verbose

SHELL_OPTS += +pc unicode
SHELL_OPTS += -config dev.config
SHELL_OPTS += -s $(PROJECT)
SHELL_OPTS += -s sync

include erlang.mk
