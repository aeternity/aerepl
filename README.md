# æREPL --- an interactive shell for Sophia

Try Sophia, test your contracts, check "what if", calculate a factorial.
Completely offline, independently from remote networks, no dockers required.

If you are not familiar with Sophia, check [its
documentation](https://github.com/aeternity/protocol/blob/master/contracts/sophia.md)
first.

# Setup

## Dependencies

Tools:

- `git`
- OTP 25 or 26
- C++ compiler

Libraries:



## Local build

Clone the repo:

```bash
git clone https://github.com/aeternity/aerepl.git
```

Set up the environment:

```bash
export ERLANG_ROCKSDB_OPTS="-DWITH_SYSTEM_ROCKSDB=ON -DWITH_LZ4=ON -DWITH_SNAPPY=ON -DWITH_BZ2=ON -DWITH_ZSTD=ON"
export CXXFLAGS="-Wno-error=shadow -Wno-deprecated-copy -Wno-redundant-move -Wno-pessimizing-move"
```

Build the project (the `make` step may need to be executed twice, see [this issue](https://github.com/aeternity/aerepl/issues/67)):

```bash
cd aerepl
make
```

Launch the REPL

```
./aerepl
```

## Docker/podman image

For a consistent setup, a docker image can be created:

```
make docker
```

Then to start the dockerized REPL:

```
docker run -i aeternity/aerepl:local
```

Use `make podman` for a podman build.

# CLI usage

æREPL usage patterns are highly inspired by
[GHCi](https://wiki.haskell.org/GHC/GHCi). Most of the syntax is a direct
adaptation of it to the needs of Sophia and FATE. This section shall provide an
overview of the most fundamental features and typical workflows. If you find
navigation clunky, please consider using
[rlwrap](https://github.com/hanslub42/rlwrap).


Main functionality of æREPL is to evaluate Sophia expressions:

```
AESO> 2 + 2
4
```

Aside from that, values can be assigned to REPL-local variables (pattern
matching is supported)

```
AESO> let x = 2
AESO> let (q, w) = (x + 1, x - 1)
AESO> q
3
AESO> w
1
```

Functions and types are supported as well

```
AESO> record point = {x : int, y : int}
AESO> function get_x(p : point) = p.x
AESO> get_x({x = 100, y = 42})
100
```

*NOTE*: in-REPL functions cannot use in-REPL variables and other functions yet.

# Typechecking

A common query is to ask about the type of an expression. This is done using the
`:type` command:

```
AESO> :type Oracle.query
(oracle('a, 'b), 'a, int, Chain.ttl, Chain.ttl) => oracle_query('a, 'b)
```

# Working with files

æREPL allows loading files to call their code and deploy contracts defined in
them. To load a file, the `:load` command may be used:

```
// File: Test.aes
namespace N =
  function f() = 100

contract C =
  entrypoint f() = N.f() + 23
```

```
AESO> :load Test.aes
AESO> N.f()
100
AESO> let c = Chain.create() : C
AESO> c.f()
123
```

If multiple files are loaded, only the most recent one is brought to the scope.
To include other loaded files into the working scope, Sophia's `include` should
be used:

```
AESO> include "List.aes"
```

Note that `:load` reloads all listed files and unloads those skipped. Yet
another useful command is `:reload`, which reloads all loaded files, preserving
the included scope.

```
AESO> :load X.aes Y.aes
AESO> Y.y()  // defined in Y.aes
"y"
AESO> X.x()  // defined in X.aes
1:1 Unbound variable: X.x
AESO> include "X.aes"
AESO> X.x()  // Now it works
"x"
```

`:load` and `:reload` clear out the scope, meaning that they will clean

- User defined variables
- User defined functions
- User defined types
- Includes
- Deployed contracts
- In-REPL state

# In-REPL state

æREPL maintains its own Sophia state which can be accessed using standard
operations `put` and `state`, as well as stateful functions. By default, the
state is set to `() : unit`. In order to change it to some other type, `:state`
command should be used:

```
AESO> :set print_unit true
AESO> state
()
AESO> :state 1000
AESO> state + 1
1001
AESO> put(state / 2)
()
AESO> state
500
```

Note that `:state` is parameterized by value, not by type. The value has to have
a fully instantiated, non-functional type. Changing the state using the `:state`
commands clears out all user-defined variables and functions --- if that is not
desired, `put` should be used to modify the state if possible.

# Configuration

æREPL can be configured interactively using the `:set` command. The syntax for
this command is `:set OPTION [ARGS]`, for example

```
AESO> :set print_unit true
```

Currently the supported options are:

| Option                  | Arguments          | Description                                                         |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `call_gas`              | non-neg int        | Determines the amount of gas to be supplied to each query.          |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `call_value`            | non-neg int        | The `value`, the amount of tokens supplied with the query.          |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `call_gas_price`        | non-neg int        | The result of `Call.gas_price`                                      |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `call_origin`           | pubkey             | The account to initiate execution of in-REPL code.                  |
|                         |                    | Defines `Call.origin` and initial `Call.caller`.                    |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `call_contract_creator` | pubkey             | The result of `Contract.creator`.                                   |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `call_fee`              | non-neg int        | The result of `Call.fee`.                                           |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `call_height`           | non-neg int        | The result of `Chain.block_height`.                                 |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `print_gas`             | `true/false`       | If `true`, REPL will print gas used for evaluating each expression. |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `print_format`          | `sophia/fate/json` | Determines the syntax used to display values.                       |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `print_unit`            | `true/false`       | Whether to display unit (`()`).                                     |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `print_type`            | `true/false`       | Whether to display the type after each eval.                        |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `loc_backwards`         | non-neg int        | Number of previous lines to be displayed when using `location`.     |
|-------------------------|--------------------|:--------------------------------------------------------------------|
| `loc_forwards`          | non-neg int        | Number of further lines to be displayed when using `location`.      |


# Output format

By default, the REPL shall display values preserving compatibility with Sophia
syntax. An exception from that rule are values containing functions which
in FATE are represented as pairs of function name and closure. In that case,
the output will take a form of `"<fun $FUNHASH>" : $TYPE` where `$TYPE` is
the type of the function (according to the Sophia code, not FATE bytecode), and
`$FUNHASH` is a hex-encoded shortcut of the original function's name hashed with
BLAKE2B (as it is stored in the bytecode). For example

```
AESO> () => ()
"<fun E3472E14>" : () => unit
```

If `print_format` is set to `fate`, the value shall be displayed as an Erlang
term that describes the exact representation of the value in the runtime.

```
AESO> :set print_format fate
AESO> record r = {x : int, y : int}
AESO> {y = 100, x = 7}
{tuple,{7,100}}
AESO> Some(1)
{variant,[0,1],1,{1}}
AESO> () => ()
{tuple,{<<227,71,46,20>>,{tuple,{}}}}
```

If set to `json`, the values will be encoded in JSON format. Information about
record fields and `datatype` constructor names shall not be dropped. Function
values are represented as strings in the same manner as in the `sophia` format.

```
AESO> :set print_format json
AESO> record r = {x: int, y : int}
AESO> ({x = 3, y = 4}, "STRING")
[
  {
    "x": 3,
    "y": 4
  },
  "STRING"
]
```

# Multiline input

To type in a multiline block of code, surround it with `:{` and `:}`:

```
AESO> :{
| let x = 123
| x
| :}
123
```

# Generic server interface

If REPL is used as a library for a different tool, its generic server can be
used for more structured interface. The server is located in
`src/aere_gen_server.erl` and implements the standard `gen_server` Erlang
behaviour.

## Startup

Use `aere_gen_server:start/1` to start up the server. `aere_gen_server:start/2`
allows to start a server under a custom name. The argument is property list with
the following fields:

- `options` --- parameter map for the repl configuration. See "Configuration"
  for more details.
- `accounts :: list(#{pubkey() => integer()})` --- list of initial accounts and
  their balances

## Call

- `quit` --- terminates the server
- `skip` --- does nothing
- `bump_nonce` --- bumps internal REPL nonce (used in naming internal variables)
- `blockchain_state` --- returns the chain state for the current session
- `theme` --- returns the current display theme
- `{type, string()}` --- parses the expression and returns its type in the
  session context
- `{state, string()}` --- parses the expression and evaluates it in the session
  context. The restult is then assigned to the session's contract store. All
  locally defined functions, variables and types are pruned.
- `{eval, string()}` --- parses the expression and evaluates it in the session
  context
- `{load, list(string())}` --- Loads files from the file system. Includes the
  first one in the list.
- `reload` --- Reloads the currently loaded files.
- `{set, Option :: atom(), Value :: list(term())}` --- changes REPL's
  configuration (see `{help, "set"}` for more details)
- `help` --- lists all available commands
- `{help, string()}` --- returns the help text for the given command
- `{lookup, atom()}` --- prints information about REPL's state or configuration
- `{disas, string()}` --- parses a reference to a function and prints its FATE
  code
- `stop` --- if at breakpoint, cancels the execution and rolls back all changes
- `{break, FileName :: string(), Line :: integer()}` --- adds a breakpoint
- `{delete_break, integer()}` --- removes a breakpoint with a given id
- `{delete_break_loc, string(), integer()}` --- removes all breakpoints from the
  given file at the given line
- `continue` --- resumes execution after a breakpoint stop
- `stepover` --- proceeds to the next line in the current execution
- `stepin` --- proceeds to the next line, or enters the function body if there
  is a function call upcoming
- `stepout` --- resumes execution until the next breakpoint or current function
  return
- `location` --- returns in-code location of current execution
- `{print_var, string()}` --- returns in-code location of where the variable was introduced
- `print_vars` --- returns values of all variables in scope
- `stacktrace` --- returns the current stacktrace
- `{set_account, pubkey(), integer()}` --- sets balance for an in-repl account
- `version` --- returns version information
- `banner` --- returns an ASCII "banner" presenting the REPL's logo and various
  version information

## Cast

- `{update_filesystem_cache, #{string() => binary()}}` --- updates REPL's
  perception of the file system to the provided map. If this is set, all include
  and file load instructions will ignore system's file system, and will use this
  map instead.

## Functions

All above calls and casts are exposed as function calls for
convenience. Additionally, the following are offered:

- `format` --- formats outputs of other commands into renderable texts to be
  used by the `render` function
- `render` --- renders a renderable output to a string using REPL's theme
- `banner` --- returns a nicely rendered banner for CLI interfaces
- `input` --- parses a text command and interprets it as one of the calls
  above. Useful for CLI-like interfaces.
- `prompt` --- proposes prompt to display in the CLI based on the REPL's
  state. For example, the default is `AESO> `.
