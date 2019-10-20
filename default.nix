{ compiler ? "ghc865"
, pkgs ? import ./nix/packages.nix {}
, isDevBuild ? true
}:

with pkgs;

let
  haskellPackages = haskell.packages.${compiler};
  source = nix-gitignore.gitignoreSource [] ./.;
  ormolu = callPackage ./nix/ormolu.nix {
    inherit fetchFromGitHub;
    inherit (haskellPackages) callCabal2nix;
  };
  devBuildInputs =
    if isDevBuild
      then with haskellPackages; [ hpack ghcid ]
      else [];
  drv = haskellPackages.callCabal2nix "validators" source {};
in
  {
    validators = drv;
    validators-shell = haskellPackages.shellFor {
      packages = p: [ drv ];
      buildInputs = with haskellPackages; [
        cabal-install
        hlint
        ormolu
      ] ++ devBuildInputs;
      withHoogle = isDevBuild;
    };
  }
