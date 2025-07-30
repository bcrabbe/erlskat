# erlskat

## Running

This project uses rebar3 for building. For detailed information about rebar3 installation and usage, please refer to the [rebar3 getting started guide](https://rebar3.org/docs/getting-started/).

We recommend using [just](https://github.com/casey/just) to run common build commands.

### Quick Start

```bash
# Install dependencies and start development shell
just deps
just shell
```

### Available Commands

Run `just` to see all available commands, or use these common ones:

- `just deps` - Downloads and compiles dependencies
- `just shell` - Starts an Erlang shell with the project loaded
- `just compile` - Compile the application
- `just test` - Run all tests
- `just release` - Build development release
- `just release-prod` - Build production release

### Manual rebar3 Commands

If you prefer to use rebar3 directly:

```bash
rebar3 get-deps
rebar3 shell
```

players can connect to the server using websockets

```bash
websocat ws://localhost:8080 | jq
```

## Build and Test

```bash
just dev-cycle  # Clean, compile, test, and build release
# Or use individual commands:
just compile
just test
```

For manual testing and debugging:

```bash
# Start manual test script
./test/manual_test.sh
```

### Manual Testing with Multiple Clients (macOS, iterm)

A little hack for quickly testing with 3 simultaneous websocket clients, you can use itermocil to automatically start the server and connect 3 clients in one cmd: 

1. Install itermocil:
```bash
brew install TomAnthony/brews/itermocil   # or: pip install itomate2
```

2. Create window configuration:
```bash
mkdir ~/.itermocil
echo '
windows:
- name: erlskat
  root: ~/p/erlskat
  layout: main-vertical          # big pane on the left, three on the right
  panes:
    - just shell
    - sleep 2; websocat ws://localhost:8080
    - sleep 2; websocat ws://localhost:8080
    - sleep 2; websocat ws://localhost:8080
' >> ~/.itermocil/erlskat.yml
```

3. Start server and connect 3 clients:
```bash
itermocil erlskat
```

## Releases

This project uses rebar3/relx for building releases. The release configuration is in `rebar.config`.

### Building Releases

```bash
# Build development release
just release

# Build production release
just release-prod

# Build and run development release
just dev

# Build and run production release
just prod
```

### Running Releases

```bash
# Run development release
just run-dev

# Run production release
just run-prod
```

### Release Structure

The release is built in the `_build/` directory with the following structure:
- `_build/default/rel/erlskat/` - Development release
- `_build/prod/rel/erlskat/` - Production release
- `_build/*/rel/erlskat/bin/` - Executable scripts
- `_build/*/rel/erlskat/lib/` - Application libraries
- `_build/*/rel/erlskat/releases/` - Release versions
- `_build/*/rel/erlskat/log/` - Log directories

### Docker

```bash
# Build and run with Docker
just docker

# Or step by step:
just docker-build
just docker-run
```
