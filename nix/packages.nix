let
  commit = "3f4144c30a6351dd79b177328ec4dea03e2ce45f";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1qg5n60n3fr6cypihnrjw451fadps5pysj5p0vvfb320mpfvlbjb";
  };
  pkgs = import nixpkgs;
in
  pkgs
