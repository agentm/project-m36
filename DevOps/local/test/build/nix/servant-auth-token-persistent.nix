{ mkDerivation, aeson-injector, base, bytestring, containers
, exceptions, fetchgit, mtl, persistent, persistent-template
, servant-auth-token, servant-auth-token-api, servant-server
, stdenv, text, time, transformers, unliftio-core, uuid
}:
mkDerivation {
  pname = "servant-auth-token-persistent";
  version = "0.7.0.0";
  src = fetchgit {
    url = "https://github.com/hughjfchen/servant-auth-token";
    sha256 = "09qq03k1xcvm018a765r168n2vl29hl1ypm05s1500q9byab9hg0";
    rev = "2f8a7a505f475decb419b89fe5fbb8e5eeb361a5";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/servant-auth-token-persistent; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson-injector base bytestring containers exceptions mtl persistent
    persistent-template servant-auth-token servant-auth-token-api
    servant-server text time transformers unliftio-core uuid
  ];
  doCheck = false;
  homepage = "https://github.com/ncrashed/servant-auth-token#readme";
  description = "Persistent backend for servant-auth-token server";
  license = stdenv.lib.licenses.bsd3;
}
