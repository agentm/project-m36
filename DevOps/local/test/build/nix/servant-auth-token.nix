{ mkDerivation, aeson-injector, base, byteable, bytestring
, containers, fetchgit, http-api-data, mtl, pwstore-fast, servant
, servant-auth-token-api, servant-server, stdenv, text, time
, transformers, uuid, wai
}:
mkDerivation {
  pname = "servant-auth-token";
  version = "0.5.6.0";
  src = fetchgit {
    url = "https://github.com/hughjfchen/servant-auth-token";
    sha256 = "00xyc4zf37v7ypyrvy9a7pq0yl7nil8pjigcjv176vs6w79f063y";
    rev = "2e3577653f9646c1e5225cf7e1e4653721e2d3e0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson-injector base byteable bytestring containers http-api-data
    mtl pwstore-fast servant servant-auth-token-api servant-server text
    time transformers uuid wai
  ];
  doCheck = false;
  homepage = "https://github.com/ncrashed/servant-auth-token#readme";
  description = "Servant based API and server for token based authorisation";
  license = stdenv.lib.licenses.bsd3;
}
