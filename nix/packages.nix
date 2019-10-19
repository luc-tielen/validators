let
  commit = "9584ce65b7cb09c001bafa1962037aa747974054";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-19.03";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "025h75b7dm5lp4a3fah19zc5x9qmficzxjfj9bdywanfl66nwc34";
  };
  pkgs = import nixpkgs;
in
  pkgs
