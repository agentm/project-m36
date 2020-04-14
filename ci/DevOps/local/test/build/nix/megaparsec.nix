{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, mtl, parser-combinators, scientific, stdenv
, text, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "7.0.5";
  sha256 = "f2dc2ea9da25f726c0650051f4fe3cec0003a97dfa37b62a3f1acdba580c1d2f";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq text weigh
  ];
  doCheck = false;
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
}
