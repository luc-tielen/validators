
# Validators

[![CircleCI](https://circleci.com/gh/luc-tielen/validators.svg?style=svg)](https://circleci.com/gh/luc-tielen/validators)


A library for validating input data in Haskell, inspired by [elm-validate](https://github.com/rtfeldman/elm-validate/).


## Developing

The easiest way to start developing is by using [Nix](https://nixos.org/nix/download.html).

```bash
$ git clone git@github.com:luc-tielen/validators.git
$ cd validators
$ nix-shell
$ cabal new-configure  # run inside the nix-shell
$ cabal new-build      # run inside the nix-shell
```

The most often used commands are also provided by a [Makefile](https://github.com/luc-tielen/validators/blob/master/Makefile).

