{ compiler ? "ghc865"
, pkgs ? import ./nix/packages.nix {}
, isDevBuild ? true
}:

(import ./. { inherit pkgs compiler isDevBuild; }).validators-shell
