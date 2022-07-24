# æREPL --- an interactive shell for Sophia

Try Sophia, test your contracts, check "what if", calculate a factorial.
Completely offline, independently from remote networks, no dockers required.

If you are not familiar with Sophia, check [its
documentation](https://github.com/aeternity/protocol/blob/master/contracts/sophia.md)
first.

# Setup

Clone the repo:

```
git clone https://github.com/aeternity/aerepl.git
```

Build the project:

```
cd aerepl
make
```

Launch the REPL

```
./aerepl
```

* Basic usage

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

Note that `:load` reloads all listed files and unloads those skipped. If you
want to add another file without unloading the rest, use `:add`. Yet another
useful command is `:reload`, which reloads all loaded files, preserving the
included scope.

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

`:load`, `:add` and `:reload` clear out the scope, meaning that they will
clean

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
commands clears out all user-defined variables as functions --- if that is not
desired, `put` should be used.

# Configuration

æREPL can be configured interactively using the `:set` command. The syntax for
this command is `:set OPTION [ARGS]`, for example

```
AESO> :set print_unit true
```

Currently the supported options are:

| Option         | Arguments     | Description                                                           |
|----------------|---------------|:----------------------------------------------------------------------|
| `display_gas`  | `true/false`  | If `true`, REPL will print gas used for evaluating each expression.   |
|----------------|---------------|:----------------------------------------------------------------------|
| `call_gas`     | non-neg int   | Determines the amount of gas to be supplied to each query.            |
|----------------|---------------|:----------------------------------------------------------------------|
| `call_value`   | non-neg int   | Sets the `value`, the amount of tokens supplied with the query.       |
|----------------|---------------|:----------------------------------------------------------------------|
| `print_format` | `sophia/fate` | If set to `sophia`, will make REPL print values using Sophia syntax.  |
|                |               | If set to `fate` it will use the representation compatible with FATE. |
|----------------|---------------|:----------------------------------------------------------------------|
| `print_unit`   | `true/false`  | Whether to display unit (`()`).                                       |

# Multiline input

To type in a multiline block of code, surround it with `:{` and `:}`:

```
AESO> :{
| let x = 123
| x
| :}
123
```
