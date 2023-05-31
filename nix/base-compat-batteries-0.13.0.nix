{ mkDerivation, base, base-compat, foldable1-classes-compat
, ghc-prim, hspec, hspec-discover, lib, OneTuple, QuickCheck
}:
mkDerivation {
  pname = "base-compat-batteries";
  version = "0.13.0";
  sha256 = "3768af40d1d64f9b0845a8ddb18e63ba3a4c765d0d96db9dc28b5b66d1232f98";
  libraryHaskellDepends = [
    base base-compat foldable1-classes-compat ghc-prim OneTuple
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  description = "base-compat with extra batteries";
  license = lib.licenses.mit;
}
