{ compiler ? "ghc865", pkgs ? import ./nix/packages.nix {} }:

(import ./. { inherit pkgs compiler; }).validators-shell
