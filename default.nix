{ compiler ? "ghc864", pkgs ? import ./nix/packages.nix {} }:

with pkgs;

let
  haskellPackages = haskell.packages.${compiler};
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {};
  };
  source = nix-gitignore.gitignoreSource [] ./.;
  drv = haskellPkgs.callCabal2nix "validators" source {};
in
  {
    validators = drv;
    validators-shell = haskellPkgs.shellFor {
      packages = p: [ drv ];
      buildInputs = with haskellPkgs; [
        cabal-install
        hpack
        hlint
        ghcid
      ];
      withHoogle = true;
    };
  }
