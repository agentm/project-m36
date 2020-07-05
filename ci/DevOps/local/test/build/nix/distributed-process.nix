{ mkDerivation, base, binary, bytestring, containers, data-accessor
, deepseq, distributed-static, exceptions, fetchgit, hashable, mtl
, network-transport, network-transport-tcp, random, rank1dynamic
, stdenv, stm, syb, template-haskell, time, transformers
}:
mkDerivation {
  pname = "distributed-process";
  version = "0.7.4";
  src = fetchgit {
    url = "https://github.com/haskell-distributed/distributed-process";
    sha256 = "0c71b3nc19zic9xiirkc41znv93f9j9qlf2kn89mjjyh9w7dazsn";
    rev = "660d554f6acd2dba8b605c84e8fa69e45708bc14";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base binary bytestring containers data-accessor deepseq
    distributed-static exceptions hashable mtl network-transport random
    rank1dynamic stm syb template-haskell time transformers
  ];
  benchmarkHaskellDepends = [
    base binary bytestring network-transport-tcp
  ];
  doCheck = false;
  homepage = "http://haskell-distributed.github.com/";
  description = "Cloud Haskell: Erlang-style concurrency in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
