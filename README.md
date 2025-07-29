# erlskat

## Running

This project uses erlang.mk for building. For detailed information about erlang.mk and its usage, please refer to the [erlang.mk getting started guide](https://erlang.mk/guide/getting_started.html).

To build and run the project:

```bash
make deps
make shell
```

- `make deps` - Downloads and compiles dependencies
- `make shell` - Starts an Erlang shell with the project loaded

players can connect to the server using websockets

```bash
websocat ws://localhost:8080 | jq
```

## Build and Test

```bash
make all
```

### Integration Tests

The project includes integration tests using BATS (Bash Automated Testing System) to test the complete happy path:

```bash
# Run all tests including integration tests
make test

# Run only integration tests
make integration

# Run integration tests with verbose output
make integration-verbose
```

For manual testing and debugging:

```bash
# Start manual test script
./test/manual_test.sh
```

See [test/README_integration.md](test/README_integration.md) for detailed information about integration testing.

## Releases

This project uses Relx for building releases. The release configuration is in `relx.config`.

### Building Releases

```bash
# Build development release (with dev_mode enabled)
make release

# Build production release (without dev_mode)
make release-prod

# Build release tarball
make release-tar
```

### Running Releases

```bash
# Build and run release in console mode
make release-run

# Run existing release
make run-release
```

### Release Structure

The release is built in the `_rel/` directory with the following structure:
- `_rel/erlskat/bin/` - Executable scripts
- `_rel/erlskat/lib/` - Application libraries
- `_rel/erlskat/releases/` - Release versions
- `_rel/erlskat/log/` - Log directories

The release tarball is created at `_rel/erlskat/erlskat-0.1.0.tar.gz` and can be deployed to production servers.


