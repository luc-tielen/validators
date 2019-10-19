
build:
	@cabal new-build

configure:
	@hpack .
	@cabal new-configure

test:
	@cabal new-run validators-test

repl:
	@cabal new-repl

lint:
	@hlint .

format:
	@find lib tests -type f -name "*.hs" | \
		xargs ormolu -m inplace

.PHONY: build configure test repl lint format
