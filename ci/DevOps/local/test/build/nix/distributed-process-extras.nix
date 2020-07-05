{ mkDerivation, ansi-terminal, base, binary, bytestring, containers
, data-accessor, deepseq, distributed-process
, distributed-process-systest, distributed-static, exceptions
, fetchgit, fingertree, ghc-prim, hashable, HUnit, mtl, network
, network-transport, network-transport-tcp, QuickCheck, rematch
, stdenv, stm, test-framework, test-framework-hunit
, test-framework-quickcheck2, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "distributed-process-extras";
  version = "0.3.5";
  src = fetchgit {
    url = "https://github.com/hughjfchen/distributed-process-extras";
    sha256 = "1fk3sxk9953wp3ya0c21l2d3yn18931bsny5d01wpn14khdy7mfj";
    rev = "5f75e9ff6d1df2c12f28d99adb15ea4267f02017";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base binary containers deepseq distributed-process exceptions
    fingertree hashable mtl stm time transformers unordered-containers
  ];
  testHaskellDepends = [
    ansi-terminal base binary bytestring containers data-accessor
    deepseq distributed-process distributed-process-systest
    distributed-static fingertree ghc-prim hashable HUnit mtl network
    network-transport network-transport-tcp QuickCheck rematch stm
    test-framework test-framework-hunit test-framework-quickcheck2 time
    transformers unordered-containers
  ];
  doCheck = false;
  homepage = "http://github.com/haskell-distributed/distributed-process-extras";
  description = "Cloud Haskell Extras";
  license = stdenv.lib.licenses.bsd3;
}
