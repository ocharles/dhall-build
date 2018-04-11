with (import <nixpkgs> {});

(haskellPackages.override {
  overrides = self: super: {
    dhall = super.callPackage ./dhall.nix {};
    formatting = super.callPackage ./formatting.nix {};
    prettyprinter = super.callPackage ./prettyprinter.nix {};
  };
}).ghcWithPackages ( hs: [ hs.dhall ] )
