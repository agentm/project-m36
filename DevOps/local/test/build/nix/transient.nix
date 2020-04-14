{ mkDerivation, base, bytestring, containers, directory, fetchgit
, mtl, primitive, random, stdenv, stm, time, transformers
, type-level-sets
}:
mkDerivation {
  pname = "transient";
  version = "0.6.4";
  src = fetchgit {
    url = "https://github.com/hughjfchen/transient";
    sha256 = "0ir60mygkcd3dsr80mv7ivlqxbmdkad1n5gb7py9szmsz7y7qkwb";
    rev = "07c164458d99975fee0f808c77fef8cde0212d03";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers directory mtl primitive random stm time
    transformers type-level-sets
  ];
  testHaskellDepends = [
    base bytestring containers directory mtl random stm time
    transformers
  ];
  doCheck = false;
  homepage = "https://github.com/transient-haskell/transient";
  description = "composing programs with multithreading, events and distributed computing";
  license = stdenv.lib.licenses.mit;
}
