
# Validators

[![CircleCI](https://circleci.com/gh/luc-tielen/validators.svg?style=svg)](https://circleci.com/gh/luc-tielen/validators)

A library for validating input data in Haskell, inspired partly by [elm-validate](https://github.com/rtfeldman/elm-validate/).


## Concepts

### Validator

The library provides a `Validator` datatype for checking assertions
on input data. A validator is parametrized by 2 type variables:

1. `err`: the type of error a validator can return,
2. `subject`: the type being inspected/validated.

Execute validators by using the `validate` function:

```haskell
data Error = TooSmall | TooBig

>>> let validator = assert (> 10) [TooSmall]
>>> validate validator 11  -- Success 11
>>> validate validator 4   -- Failure [TooSmall]
```

Validators return a `Validation` type as result, which can be
further chained or checked for the final result. This is explained in
the [Validation](#Validation) section.

Validators can also be combined using the `Semigroup` instance,
resulting in a validator that accumulates errors from both sub-validators:

```haskell
>>> let validator = maxSize 3 [TooBig] <> minSize 2 [TooSmall]
>>> validate validator [1, 2, 3]     -- Success [1,2,3]
>>> validate validator [1, 2, 3, 4]  -- Failure [TooBig]
>>> validate validator [1]           -- Failure [TooSmall]
```

The library provides helper functions for commonly used validation checks.
See the docs for a complete overview which validators are available.
Custom validation checks can be created using the low-level `assert`/`refute`
functions.


### Validation

Once a `Validator` has been executed, it will return a `Validation` data type.
This value will contain either all accumulated errors found during the checking
of assertions, or the given input value.

Validations are composed using their `Applicative` instance. This can be useful
for example when validating product types. Like with `Validator`, this will also
accumulate all encountered errors.

The following is an example for validating a `Person` data type:

```haskell
data PersonError = MissingName | InvalidAge

type Name = String
type Age = Int
data Person = Person Name Age

>>> let nameValidator = ifEmpty [MissingName]
>>> let ageValidator = assert (> 0) [InvalidAge]
>>> Person <$> validate nameValidator "" <*> validate ageValidator (subtract 1)       -- Failure [MissingName, InvalidAge]
>>> Person <$> validate nameValidator "Alice" <*> validate ageValidator (subtract 1)  -- Failure [InvalidAge]
>>> Person <$> validate nameValidator "Alice" <*> validate ageValidator 25            -- Success (Person "Alice" 25)
```


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

