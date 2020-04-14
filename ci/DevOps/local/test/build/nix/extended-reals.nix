{ mkDerivation, base, deepseq, hashable, HUnit, QuickCheck, stdenv
, tasty, tasty-hunit, tasty-quickcheck, tasty-th
}:
mkDerivation {
  pname = "extended-reals";
  version = "0.2.3.0";
  sha256 = "7c8ac733ef09cb37c0a9816e7765ab6fe46551ee58445c00fc6c52a37eef169c";
  revision = "5";
  editedCabalFile = "1qm0xrlklg0849a2ydg2gh1fimxknmwpy7xmh4p9qsy5dlyj7516";
  libraryHaskellDepends = [ base deepseq hashable ];
  testHaskellDepends = [
    base deepseq HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
    tasty-th
  ];
  doCheck = false;
  homepage = "https://github.com/msakai/extended-reals/";
  description = "Extension of real numbers with positive/negative infinities";
  license = stdenv.lib.licenses.bsd3;
}
