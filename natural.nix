{ mkDerivation, base, checkers, hedgehog, lens, QuickCheck, stdenv
, tasty, tasty-hedgehog, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "natural";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens ];
  testHaskellDepends = [
    base checkers hedgehog lens QuickCheck tasty tasty-hedgehog
    tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "https://github.com/qfpl/natural";
  description = "Natural number";
  license = stdenv.lib.licenses.bsd3;
}
