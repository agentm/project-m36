{ mkDerivation, async, attoparsec, base, binary, bytestring
, conduit, conduit-extra, containers, fetchgit, hpack, HUnit
, network-conduit-tls, network-uri, QuickCheck, stdenv, stm, tasty
, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "net-mqtt";
  version = "0.2.4.1";
  src = fetchgit {
    url = "https://github.com/dustin/mqtt-hs";
    sha256 = "1vvvf1ijg9v73rwxjyjbh7kxvravracbslzn0p9mg2sz17l3dy9r";
    rev = "26e7acc7b95fccf8723a83ecfd0bec9d5e697f79";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base binary bytestring conduit conduit-extra
    containers network-conduit-tls network-uri stm text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    async attoparsec base binary bytestring conduit conduit-extra
    containers network-conduit-tls network-uri stm text
  ];
  testHaskellDepends = [
    async attoparsec base binary bytestring conduit conduit-extra
    containers HUnit network-conduit-tls network-uri QuickCheck stm
    tasty tasty-hunit tasty-quickcheck text
  ];
  doCheck = false;
  prePatch = "hpack";
  homepage = "https://github.com/dustin/mqtt-hs#readme";
  description = "An MQTT Protocol Implementation";
  license = stdenv.lib.licenses.bsd3;
}
