{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.hpack
    pkgs.haskellPackages.stack
    pkgs.haskellPackages.haskell-language-server
  ];
}