{ mkDerivation, base, binary, HUnit, stdenv, test-framework
, test-framework-hunit
}:
mkDerivation {
  pname = "rank1dynamic";
  version = "0.4.0";
  sha256 = "3c424bfe52b7d4766fd66ea34c204cf920b146455711d8d10d580ca6c175ab1d";
  revision = "1";
  editedCabalFile = "1idh1iz15pzdhrhy19584i9ahz41ijbmf56wbb2wns2kipy6w9lr";
  libraryHaskellDepends = [ base binary ];
  testHaskellDepends = [
    base HUnit test-framework test-framework-hunit
  ];
  doCheck = false;
  homepage = "http://haskell-distributed.github.com";
  description = "Like Data.Dynamic/Data.Typeable but with support for rank-1 polymorphic types";
  license = stdenv.lib.licenses.bsd3;
}
