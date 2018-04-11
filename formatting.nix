{ mkDerivation, array, base, bytestring, clock, ghc-prim, hspec
, integer-gmp, old-locale, scientific, semigroups, stdenv, text
, time, transformers
}:
mkDerivation {
  pname = "formatting";
  version = "6.3.2";
  sha256 = "0e4fb8b5e6b996bcaedc503409b4fa9a88244a7610f1ebab57383426fcf1b2c0";
  libraryHaskellDepends = [
    array base bytestring clock ghc-prim integer-gmp old-locale
    scientific semigroups text time transformers
  ];
  testHaskellDepends = [ base hspec semigroups ];
  description = "Combinator-based type-safe formatting (like printf() or FORMAT)";
  license = stdenv.lib.licenses.bsd3;
}
