{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, directory, entropy, fetchgit, filepath, hashable
, HTTP, iproute, mtl, network, network-info, network-uri, primitive
, process, random, stdenv, stm, TCache, text, time, transformers
, transient, vector, websockets
}:
mkDerivation {
  pname = "transient-universe";
  version = "0.5.2";
  src = fetchgit {
    url = "https://github.com/hughjfchen/transient-universe";
    sha256 = "1l3rb0j82lgjfd974jba1iplbjxybxm0mr5l7s0f26awfjp0mbqa";
    rev = "f9a08d2d089af3e06a4772fcff83c9c35045f1a5";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive containers directory entropy
    filepath hashable HTTP iproute mtl network network-info network-uri
    primitive process random stm TCache text time transformers
    transient vector websockets
  ];
  executableHaskellDepends = [
    aeson base bytestring case-insensitive containers directory
    filepath hashable HTTP mtl network network-info network-uri process
    random stm TCache text time transformers transient vector
    websockets
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive containers directory
    filepath hashable HTTP mtl network network-info network-uri process
    random stm TCache text time transformers transient vector
    websockets
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/transient-haskell/transient-universe";
  description = "Remote execution and map-reduce: distributed computing for Transient";
  license = stdenv.lib.licenses.mit;
}
