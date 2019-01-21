# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "typerep-map";
  buildInputs = [
    haskell.compiler.ghc844
    pkgs.cabal-install
  ];
}
