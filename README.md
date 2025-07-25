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


