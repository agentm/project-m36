{ compiler ? "ghc927"
, sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  dontCheck = pkgs.haskell.lib.dontCheck;
  needsCocoa = drv:
    if pkgs.stdenv.isDarwin
    then drv.overrideDerivation (old:
      { buildInputs = [ pkgs.darwin.apple_sdk.frameworks.Cocoa ] ++ old.buildInputs; }
    )
    else drv;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        doCheck = false;
        doHaddock = false;
      });
      wai = dontCheck super.wai;
      wai-websockets = dontCheck super.wai-websockets;
      scotty = dontCheck super.scotty;
      warp = dontCheck super.warp;
      warp-tls = dontCheck super.warp-tls;
      retry = dontCheck super.retry;
      uniplate = dontCheck super.uniplate;
      filepattern = dontCheck super.filepattern;
      http-api-data = dontCheck super.http-api-data;
      deferred-folds = dontCheck super.deferred-folds;

      Cabal = dontCheck (self.callHackageDirect {
          pkg = "Cabal";
	  ver = "3.6.3.0";
          sha256 = "sha256-c1U5klS7V9eB6jwt69+vlR61prrrnTE1UXNCTgiyUkQ=";
        } {});
      curryer-rpc = self.callHackageDirect {
                      pkg = "curryer-rpc";
		      ver = "0.3.0";
		      sha256 = "sha256-sKRHx49yhfvLxMUPRHs2apZ1v0iGbZm/JTTjXJKVD9M="; } {};

      streamly = self.callHackageDirect {
                   pkg = "streamly";
		   ver = "0.9.0";
		   sha256 = "sha256-eOxVb8qQjZDo1+S7CStqYSExOg2QHWkMY+zlOYqwZak="; } {};

      streamly-core = self.callHackageDirect {
      		   pkg = "streamly-core";
		   ver = "0.1.0";
		   sha256 = "sha256-hoSV6Q2+X5a7hFnJAArqNPjcMaCVyX9Vz4FcxeJ+jgI="; } {};
		   
      streamly-bytestring = self.callHackageDirect {
      		   pkg = "streamly-bytestring";
		   ver = "0.2.0";
		   sha256 = "sha256-9mZiEVPtB0fU65nTcx2CX/3GoXbpZs5BXOgZyT+R2AY="; } {};

      lockfree-queue = self.callHackageDirect {
      		   pkg = "lockfree-queue";
		   ver = "0.2.4";
		   sha256 = "sha256-h1s/tiBq5Gzl8FtenQacmxJp7zPJPnmZXtKDPvxTSa4="; } {};

      unicode-data = self.callHackageDirect {
                      pkg = "unicode-data";
		      ver = "0.2.0";
		      sha256 = "14crb68g79yyw87fgh49z2fn4glqx0zr53v6mapihaxzkikhkkc3";
		     } {};

      winery = self.callHackageDirect {
                 pkg = "winery";
		 ver = "1.4";
                 sha256 = "sha256-ApJg6Qc25UyNZtSN52N9OrUQ/9K4w258oSE5BokO4tE=";
		 } {};

      hashable = self.callHackageDirect {
                    pkg = "hashable";
		    ver = "1.4.2.0";
		    sha256 = "sha256-OVHUu/JroAHC3fG/UAZbWNCcV/zq09EBkaHC9ago0tQ=";
		    } {};

      linux-xattr = doJailbreak (self.callHackageDirect {
      		      pkg = "linux-xattr";
		      ver = "0.1.1.0";
		      sha256 = "sha256-l35drTPGKwpip77/3BwDr7Eo0Arjfp34Cc3oRGyf+po=";
                    } {});

      text = self.callHackageDirect {
                pkg = "text";
		ver = "2.0.2";
                sha256 = "sha256-SJPgiC0LOE3x5RBxZ0lxyODnGy3tzbrOKdTJrGZyKb4=";		
      } {};

      hedgehog = dontCheck (self.callHackageDirect {
      	       pkg = "hedgehog";
	       ver = "1.1.2";
	       sha256 = "sha256-O9r3Djws1ZN4deeymQw/Wdb7d/3GYXagcHtA0ghV6X4=";
      
      } {});

      parsec = dontCheck (self.callHackageDirect {
      	     pkg = "parsec";
	     ver = "3.1.16.0";
             sha256 = "sha256-zI+x9BAbrRTcZZf0LSxyjy7VsYeSPXMZ1ByT3c0FaSw=";
	    } {});
    
      project-m36 = ((self.callCabal2nixWithOptions "project-m36" ./. "-f-haskell-scripting" {}));
    };
  };
in
{
  project = haskellPackages.project-m36;

  shell = haskellPackages.shellFor {
    packages = p: [
      p.project-m36
    ];
    buildInputs = [
      haskellPackages.ghcid
      haskellPackages.hlint
      pkgs.docker
    ];
    withHoogle = true;
  };
}
