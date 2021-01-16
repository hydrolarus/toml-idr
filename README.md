<!--
SPDX-FileCopyrightText: 2021 The toml-idr developers

SPDX-License-Identifier: CC0-1.0
-->

# `toml-idr`

A [TOML](https://toml.io/) parser for [Idris 2](https://github.com/idris-lang/Idris2).

## State of implementation

Supported values:
- [x] integers (without `_` separators **TODO**)
- [x] floats (without `_` separators **TODO**)
- [x] strings (only `\"` escape is supported for now **TODO**)
- [x] booleans
- [x] arrays
- [x] inline tables
- [ ] Offset Date-Time
- [ ] Local Date-Time
- [ ] Local Date
- [ ] Local Time

Arrays of tables (`[[thing]]`) are currently not supported. **Work in progress**

## Installation

At least version `0.3.0` of the Idris 2 compiler is required.

```sh
idris2 --install toml.ipkg
```

## Usage

After installing the package, add `toml` to the `depends` section in the `.ipkg` file.

To parse a TOML file, the `parseTOML` function in the `Language.TOML` module
can be used.

Values are represented with the `Value` type.

A file is represented as one `Table`. A `Table` is a `SortedMap`, so
`lookup`, `insert` and friends will work as usual.
To lookup nested keys (such as `["animal", "type", "name"]`) the `lookupNested`
function can be used.

## Development

To run the tests, run `make tests`. The tests need the
[`test` package](https://github.com/tiatomee/test-idr) to be installed

The command `make clear` will clean the build directory.

## License

All code is licensed under the [MPL-2.0](LICENSES/MPL-2.0.txt).

All files that are not properly copyrightable are in the public domain, using
the [CC0 license](LICENSES/CC0-1.0.txt).

This project aims to be [REUSE compliant](https://reuse.software/).