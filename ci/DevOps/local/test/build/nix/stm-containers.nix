{ mkDerivation, base, deferred-folds, focus, foldl, free, hashable
, HTF, list-t, QuickCheck, quickcheck-text, rerebase, stdenv
, stm-hamt, transformers
}:
mkDerivation {
  pname = "stm-containers";
  version = "1.1.0.4";
  sha256 = "d43346bfadbe5ccc33c0903a7dfbce8f48657184176fcefa11174ff9d2269655";
  libraryHaskellDepends = [
    base deferred-folds focus hashable list-t stm-hamt transformers
  ];
  testHaskellDepends = [
    deferred-folds focus foldl free HTF list-t QuickCheck
    quickcheck-text rerebase
  ];
  doCheck = false;
  homepage = "https://github.com/nikita-volkov/stm-containers";
  description = "Containers for STM";
  license = stdenv.lib.licenses.mit;
}
