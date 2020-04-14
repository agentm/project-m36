{ mkDerivation, base, binary, bytestring, containers, deepseq
, rank1dynamic, stdenv
}:
mkDerivation {
  pname = "distributed-static";
  version = "0.3.9";
  sha256 = "ee9a9595c20c6456b2b14e579349546f2e0cda46706a72858ec651dbd01490d9";
  libraryHaskellDepends = [
    base binary bytestring containers deepseq rank1dynamic
  ];
  doCheck = false;
  homepage = "http://haskell-distributed.github.com";
  description = "Compositional, type-safe, polymorphic static values and closures";
  license = stdenv.lib.licenses.bsd3;
}
