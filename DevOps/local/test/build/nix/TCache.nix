{ mkDerivation, base, bytestring, containers, directory, fetchgit
, hashtables, hpack, mtl, old-time, RefSerialize, stdenv, stm, text
}:
mkDerivation {
  pname = "TCache";
  version = "0.13.0.0";
  src = fetchgit {
    url = "https://github.com/hughjfchen/TCache";
    sha256 = "03c7g726n1hwhf7kn07j22p4w7gac1kkhnijplhs6nq64g1cwjwp";
    rev = "7f5258a501c844195bf18ae2f263ee795bcaf894";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers directory hashtables mtl old-time
    RefSerialize stm text
  ];
  libraryToolDepends = [ hpack ];
  doCheck = false;
  preConfigure = "hpack";
  homepage = "https://github.com/agocorona/TCache#readme";
  description = "A Transactional cache with user-defined persistence";
  license = stdenv.lib.licenses.bsd3;
}
