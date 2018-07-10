# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "typerep-map";
  buildInputs = [
    haskell.compiler.ghc843
    pkgs.cabal-install
  ];
}
