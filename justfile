# Erlskat build commands using rebar3
# Install just: https://github.com/casey/just

# Default recipe - show available commands
default:
    @just --list

# Install dependencies
deps:
    rebar3 get-deps

# Compile the application
compile:
    rebar3 compile

# Start development shell
shell:
    rebar3 shell

# Run unit tests
eunit:
    rebar3 eunit

# Run common tests
ct:
    rebar3 ct

# Run all tests (unit + common)
test:
    rebar3 eunit

# Run Elvis linting (if configured)
lint:
    rebar3 elvis rock

# Build development release
release:
    rebar3 release

# Build production release
release-prod:
    rebar3 as prod release

# Run development release in console mode
run-dev:
    _build/default/rel/erlskat/bin/erlskat console

# Run production release in console mode
run-prod:
    _build/prod/rel/erlskat/bin/erlskat console

# Clean build artifacts
clean:
    rebar3 clean

# Deep clean (remove deps and build artifacts)
distclean:
    rm -rf _build deps

# Build and run development release
dev: release run-dev

# Build and run production release
prod: release-prod run-prod

# Integration tests (requires BATS, websocat, jq)
integration:
    make integration

# Verbose integration tests
integration-verbose:
    make integration-verbose

# Docker build
docker-build:
    docker build -t erlskat .

# Docker run
docker-run:
    docker run --rm -p 8080:8080 erlskat

# Docker build and run
docker: docker-build docker-run

# Full development cycle
dev-cycle: clean compile test release

# Full production cycle
prod-cycle: clean compile test release-prod

# Show rebar3 version
version:
    rebar3 version
