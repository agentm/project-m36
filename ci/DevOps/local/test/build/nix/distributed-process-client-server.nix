{ mkDerivation, ansi-terminal, base, binary, containers, deepseq
, distributed-process, distributed-process-async
, distributed-process-extras, distributed-process-systest
, exceptions, fetchgit, fingertree, ghc-prim, hashable, HUnit, mtl
, network, network-transport, network-transport-tcp, rematch
, stdenv, stm, test-framework, test-framework-hunit, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "distributed-process-client-server";
  version = "0.2.5.1";
  src = fetchgit {
    url = "https://github.com/hughjfchen/distributed-process-client-server";
    sha256 = "1ld4madf8fbv640rv0gyp1sbfac687k9iikaqj23rdvr2bi22h2v";
    rev = "4648b7f7cce67c0af47bd0f56915bc6d59131225";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base binary containers deepseq distributed-process
    distributed-process-async distributed-process-extras exceptions
    fingertree hashable mtl stm time transformers unordered-containers
  ];
  testHaskellDepends = [
    ansi-terminal base binary containers deepseq distributed-process
    distributed-process-async distributed-process-extras
    distributed-process-systest exceptions fingertree ghc-prim HUnit
    mtl network network-transport network-transport-tcp rematch stm
    test-framework test-framework-hunit transformers
  ];
  doCheck = false;
  homepage = "http://github.com/haskell-distributed/distributed-process-client-server";
  description = "The Cloud Haskell Application Platform";
  license = stdenv.lib.licenses.bsd3;
}
