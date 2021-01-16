# SPDX-FileCopyrightText: 2021 The toml-idr developers
#
# SPDX-License-Identifier: CC0-1.0

.PHONY: toml
toml:
	@printf "Building toml-idr ..."
	@output=`idris2 --build toml.ipkg 2>&1` || (printf '\n%s\n' "$$output" && false)
	@printf " done!\n"

.PHONY: install
install: toml
	@printf "Installing toml-idr ..."
	@output=`idris2 --install toml.ipkg 2>&1` || (printf '\n%s\n' "$$output" && false)
	@printf " done!\n"

.PHONY: tests
tests: install
	@printf "Building test package ..."
	@output=`idris2 --build tests.ipkg 2>&1` || (printf '\n%s\n' "$$output" && false)
	@printf " done!\n"
	@echo "Running tests:"
	@./build/exec/tests

.PHONY: clear
clear:
	rm build -rf