{ mkDerivation, ansi-terminal, base, binary, containers
, data-accessor, deepseq, distributed-process
, distributed-process-systest, exceptions, fetchgit, fingertree
, hashable, mtl, network, network-transport, network-transport-tcp
, rematch, stdenv, stm, test-framework, test-framework-hunit, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "distributed-process-async";
  version = "0.2.6";
  src = fetchgit {
    url = "https://github.com/hughjfchen/distributed-process-async";
    sha256 = "00sh8glxf21l56s07v5jp13gxq2v5cp8w2b7snq3dc405mrl2jk9";
    rev = "73fcda3c254538db2386184530203c8aac26b905";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base binary containers data-accessor deepseq distributed-process
    exceptions fingertree hashable mtl stm time transformers
    unordered-containers
  ];
  testHaskellDepends = [
    ansi-terminal base binary deepseq distributed-process
    distributed-process-systest exceptions network network-transport
    network-transport-tcp rematch stm test-framework
    test-framework-hunit transformers
  ];
  doCheck = false;
  homepage = "http://github.com/haskell-distributed/distributed-process-async";
  description = "Cloud Haskell Async API";
  license = stdenv.lib.licenses.bsd3;
}
