{ nixpkgs ? import <nixpkgs> {} }:

((nixpkgs.haskellPackages.override {
  overrides = self: super: {
    dhall =
      super.callPackage
        ( nixpkgs.fetchFromGitHub {
            repo = "dhall-haskell";
            owner = "dhall-lang";
            rev = "0091b09183599198d8e77c056fb3887354b243d1";
            sha256 = "0wp0fh60awmfs1cby2s8r6x7i1y3mzyxasfra12m2cn6fdzw5kqr";
          }
        )
        {};
  };
}).callCabal2nix "dhall-to-cabal" ./. {}).env
