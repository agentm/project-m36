{ compiler ? "ghc928"
, sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  needsCocoa = drv:
    if pkgs.stdenv.isDarwin
    then drv.overrideDerivation (old:
      { buildInputs = [ pkgs.darwin.apple_sdk.frameworks.Cocoa ] ++ old.buildInputs; }
    )
    else drv;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      curryer-rpc = self.callHackageDirect {
                      pkg = "curryer-rpc";
		      ver = "0.3.6";
		      sha256 = "sha256-GgYxb3eBhANGMdN3FlMgD9vZUqoDsz89OFIBxwK4YtY="; } {};

      streamly = self.callHackageDirect {
                   pkg = "streamly";
		   ver = "0.10.1";
		   sha256 = "sha256-9tWZ/8YteD9ljhEmj8oYKIAyFcbQflX0D20j/NTe3qM="; } {};

      streamly-core = self.callHackageDirect {
      		    pkg = "streamly-core";
		    ver = "0.2.2";
  		    sha256 = "sha256-Ggo5ius3dp/TJFfrZSk31A6gSZHA6kLMtxFKe9MIvqQ="; } {};
		    
      streamly-bytestring = self.callHackageDirect {
                    pkg = "streamly-bytestring";
		    ver = "0.2.2";
  		    sha256 = "sha256-E/sMAvaJ5zGYwb5KAXa2KQo3FqyB+T2mRO6zOTCXpoY="; } {};		    

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

      barbies-th = self.callHackageDirect {
      	      pkg = "barbies-th";
	      ver = "0.1.10";
  	      sha256 = "sha256-cnTevB2qoEBMmGbqypQwJzPVF6z3cOXADbWF8OKQGAo=";	      
      } {};

      scotty = self.callHackageDirect {
      	     pkg = "scotty";
	     ver = "0.22";
             sha256 = "sha256-DY4lKmAmqGTrzKq93Mft9bu9Qc0QcsEVpKzgoWcBL2I=";
	     } {};

      wai = self.callHackageDirect {
            pkg = "wai";
	    ver = "3.2.4";
            sha256 = "sha256-NARmVhT5G1eMdtMM1xp7RFpevunThAB4tltCMih+qu8=";
	    } {};

      wai-extra = self.callHackageDirect {
           pkg = "wai-extra";
	   ver = "3.1.14";
           sha256 = "sha256-wMI9eTituRbMvYvbcA9pgIwFxkbdL1+2Xw78lghfWaU=";
	   } {};
    
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
