{ mkDerivation, aeson, base, bifunctors, containers, deepseq
, hashable, HUnit, lens, QuickCheck, quickcheck-text, scientific
, servant-docs, stdenv, swagger2, tasty, tasty-hunit
, tasty-quickcheck, text, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson-injector";
  version = "1.1.2.0";
  sha256 = "a4b53c8715e71dd8987e5500b98508497fa8c2ab8573418948f8f0694d33a014";
  libraryHaskellDepends = [
    aeson base bifunctors deepseq hashable lens servant-docs swagger2
    text unordered-containers
  ];
  testHaskellDepends = [
    aeson base containers HUnit lens QuickCheck quickcheck-text
    scientific swagger2 tasty tasty-hunit tasty-quickcheck text vector
  ];
  doCheck = false;
  description = "Injecting fields into aeson values";
  license = stdenv.lib.licenses.mit;
}
