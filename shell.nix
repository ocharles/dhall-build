{ nixpkgs ? import <nixpkgs> {} }:

let
  haskell =
    nixpkgs.haskellPackages.override {
      overrides =
        nixpkgs.haskell.lib.packagesFromDirectory { directory = ./nix; };
    };

in
( haskell.callCabal2nix "dhall-build" ./. {} ).env
