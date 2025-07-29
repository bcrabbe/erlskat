# Makefile (erlang.mk) — fixed for modern Erlang.mk
PROJECT             = erlskat
PROJECT_DESCRIPTION = New project
PROJECT_VERSION     = 0.1.0
RELX_TAR            = 0

# Remove -Werror that Erlang.mk now injects by default
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

# ------------ Runtime, build-time and shell deps ------------
SHELL_DEPS = sync
dep_sync = hex 0.4.1
LOCAL_DEPS = sasl

# Release dependencies
BUILD_DEPS += relx
REL_DEPS += relx

# Release configuration
RELX_CONFIG = $(CURDIR)/relx.config
RELX_OUTPUT_DIR = $(CURDIR)/_rel
RELX_OPTS = -d true  # Enable dev_mode for development

BUILD_DEPS += elvis_mk
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

SHELL_OPTS += +pc unicode
SHELL_OPTS += -config dev.config
SHELL_OPTS += -s $(PROJECT)
SHELL_OPTS += -s sync

include erlang.mk

# Convenience aggregate target
all:: elvis eunit ct
format:: elvis
test:: eunit ct
run:: deps shell

# Release targets
release:: deps
	@echo "Building release..."
	$(MAKE) rel

release-run:: release
	@echo "Running release..."
	$(MAKE) run-release

run-release::
	@echo "Starting release in console mode..."
	./_rel/$(PROJECT)/bin/$(PROJECT) console

release-clean::
	@echo "Cleaning release artifacts..."
	rm -rf _rel/
	rm -f $(PROJECT)-*.tar.gz

# Integration tests
integration::
	@echo "Running integration tests..."
	@if command -v bats >/dev/null 2>&1; then \
		bats test/integration_happy_path.bats; \
	else \
		echo "BATS not found. Please install bats-core:"; \
		echo "  brew install bats-core  # macOS"; \
		echo "  sudo apt-get install bats  # Ubuntu/Debian"; \
		exit 1; \
	fi

integration-verbose::
	@echo "Running integration tests with verbose output..."
	@if command -v bats >/dev/null 2>&1; then \
		bats test/integration_happy_path.bats --verbose; \
	else \
		echo "BATS not found. Please install bats-core:"; \
		echo "  brew install bats-core  # macOS"; \
		echo "  sudo apt-get install bats  # Ubuntu/Debian"; \
		exit 1; \
	fi
