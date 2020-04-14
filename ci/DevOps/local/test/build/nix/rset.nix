{ mkDerivation, base, QuickCheck, safe, stdenv }:
mkDerivation {
  pname = "rset";
  version = "1.0.0";
  sha256 = "f80e4f1092be752e7fc6ba965626d616b477a812c4425cab0c1456342e4c39b4";
  libraryHaskellDepends = [ base safe ];
  testHaskellDepends = [ base QuickCheck safe ];
  doCheck = false;
  homepage = "https://github.com/lovasko/rset";
  description = "Range set";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
