{ mkDerivation, aeson, aeson-injector, base, fetchgit, lens
, raw-strings-qq, servant, servant-docs, servant-swagger, stdenv
, swagger2, text
}:
mkDerivation {
  pname = "servant-auth-token-api";
  version = "0.5.3.0";
  src = fetchgit {
    url = "https://github.com/hughjfchen/servant-auth-token-api";
    sha256 = "1wmf4x663vkjsdhgamh2src9rhc1lsgd16rf2nz9r79nc9pb0ykr";
    rev = "9f4647e70d55b0789c8da52863d44bd785252c41";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson aeson-injector base lens raw-strings-qq servant servant-docs
    servant-swagger swagger2 text
  ];
  doCheck = false;
  homepage = "https://github.com/ncrashed/servant-auth-token-api#readme";
  description = "Servant based API for token based authorisation";
  license = stdenv.lib.licenses.bsd3;
}
