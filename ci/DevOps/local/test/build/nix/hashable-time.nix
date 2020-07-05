{ mkDerivation, base, hashable, stdenv, time }:
mkDerivation {
  pname = "hashable-time";
  version = "0.2.0.2";
  sha256 = "fa61f7fbd493b5a54f2aeb10e0c1c4144111ecf34e74080d12c5738ce925fee0";
  revision = "3";
  editedCabalFile = "1dr7ak803ngrhpv43dy25jm18gfzn02gzd3hm31dzcjv3mxsmbrk";
  libraryHaskellDepends = [ base hashable time ];
  doCheck = false;
  description = "Hashable instances for Data.Time";
  license = stdenv.lib.licenses.bsd3;
}
