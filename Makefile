
build:
	@cabal new-build

configure:
	@hpack .
	@cabal new-configure

tests: unit_tests doc_tests

unit_tests:
	@cabal new-run validators-test

doc_tests:
	@cabal new-run validators-doctest

repl:
	@cabal new-repl

lint:
	@hlint .

format:
	@find lib tests -type f -name "*.hs" | \
		xargs ormolu -m inplace

format_check:
	@find lib tests -type f -name "*.hs" | \
		xargs ormolu -m check

docs:
	@cabal new-haddock

.PHONY: build configure tests unit_tests doc_tests repl lint format format_check docs
