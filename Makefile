
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

.PHONY: build configure test repl lint
