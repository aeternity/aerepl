# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.3.2] - 2024.05.29

### Fixed

- The issue of large build size by setting `include_src` to `false` for `prod`
build, and removing unnecessary dependencies.

## [3.3.1] - 2024.04.12

### Fixed

- Compilation error due to a bug in enacl library.

## [3.3.0] - 2024.02.03

### Added

- `format` generic server call for formatting outputs as renderables
- `version` generic server call
- `set_account` generic server call for setting balance of arbitrary account
- `stop` command to quit debugging state and revert the call
- Option `call_gas_price` to set result of `Call.gas_price`
- Option `call_origin` to set the account to execute the repl query (affects
  `Call.origin` and `Call.caller`)
- Option `call_contract_creator` to set result of `Contract.creator`
- Option `call_fee` to set result of `Call.fee`
- Option `call_height` to set result of `Chain.block_height`
- Startup parameter `accounts` to specify initial account balances
- CLI arguments to set the newly added `accounts` parameter

### Changed

- Generic server now returns structured data and errors which have to be
  formatted manually using `format` call
- `location` display has been improved
- `display_gas` is renamed to `print_gas` for consistency
- Fixed numerous bugs and crashes
- Adjusted output of `:location`

### Removed

- In-repl Sophia functions

## [3.2.0] - 2023.12.19

### Added

- Option to return rendered results
- Option to return raw erlang values for successful calls to the gen server
- Several options to configure printing format

### Changed

- Themed rendering now returns bytestrings
- `print` is renamed to `lookup`
- `aere_repl` more often returns a tuple `{Result, repl_state()}`
- CLI uses REPL supervisor directly

### Removed

- In-repl functions

## [3.1.1] - 2023.12.10

### Added

- Command `print_vars` to list all values of all variables at a breakpoint

## [3.1.0] - 2023.08.29

### Added

- File system cache

## [3.0.0] - 2023.07.17

### Added

- Debugger integration
- REPL meta-states: normal, break, abort

### Changed

- Rendering of colored messages

## [2.3.0] - 2023.09.12

### Changed

- Reworked file loading and including to resemble GHCi's behaviour

## [2.2.0] - 2022.09.04

### Added

- Disassembling features

### Changed

- Fixed parsing errors
- Fixed bugs in printing outputs

## [2.1.0] - 2022.08.29

### Added

- Dockerfile
- `help` instruction
- `print` instruction

### Changed

- Refactored the project structure
- Fixed command parsing

## [2.0.0] - 2022.08.05

### Changed

- Massively refactored and cleaned code
- The REPL operates directly on the FATE engine

### Removed

- Most of inline definitions such as types, functions, variables

## [1.2.0] - 2020.04.02

### Added

- Added [CHANGELOG.md](CHANGELOG.md)

### Changed

- Updated to [Sophia 4.3.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#430)

[Unreleased]: https://github.com/aeternity/aerepl/compare/v3.3.2...HEAD
[3.3.2]: https://github.com/aeternity/aerepl/compare/v3.3.1...v3.3.2
[3.3.1]: https://github.com/aeternity/aerepl/compare/v3.3.0...v3.3.1
[3.3.0]: https://github.com/aeternity/aerepl/compare/v3.2.0...v3.3.0
[3.2.0]: https://github.com/aeternity/aerepl/compare/v3.1.1...v3.2.0
[3.1.1]: https://github.com/aeternity/aerepl/compare/v3.1.0...v3.1.1
[3.1.0]: https://github.com/aeternity/aerepl/compare/v3.0.0...v3.1.0
[3.0.0]: https://github.com/aeternity/aerepl/compare/v2.3.0...v3.0.0
[2.3.0]: https://github.com/aeternity/aerepl/compare/v2.2.0...v2.3.0
[2.2.0]: https://github.com/aeternity/aerepl/compare/v2.1.0...v2.2.0
[2.1.0]: https://github.com/aeternity/aerepl/compare/v2.0.0...v2.1.0
[2.0.0]: https://github.com/aeternity/aerepl/compare/v1.2.0...v2.0.0
[1.2.0]: https://github.com/aeternity/aerepl/releases/tag/v1.2.0
