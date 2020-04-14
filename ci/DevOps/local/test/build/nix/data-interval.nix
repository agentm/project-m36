{ mkDerivation, base, ChasingBottoms, containers, deepseq
, extended-reals, hashable, HUnit, lattices, QuickCheck, stdenv
, syb, tasty, tasty-hunit, tasty-quickcheck, tasty-th
}:
mkDerivation {
  pname = "data-interval";
  version = "2.0.1";
  sha256 = "5331e2df31eba3f5eb1d097a38fc5428dae56af0c0a7d71bd00491bcf81ee76f";
  libraryHaskellDepends = [
    base containers deepseq extended-reals hashable lattices
  ];
  testHaskellDepends = [
    base ChasingBottoms containers deepseq hashable HUnit lattices
    QuickCheck syb tasty tasty-hunit tasty-quickcheck tasty-th
  ];
  doCheck = false;
  description = "Interval datatype, interval arithmetic and interval-based containers";
  license = stdenv.lib.licenses.bsd3;
}
