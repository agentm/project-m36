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
		      ver = "0.3.3";
		      sha256 = "sha256-IzUOtMOfsnDG9BBvXnlywIMAUntctX0jNPZxzOQnmHo="; } {};

      streamly = self.callHackageDirect {
                   pkg = "streamly";
		   ver = "0.10.0";
		   sha256 = "sha256-QkqfJ7ta+Odfv5wYL+SvOpM6ZmVTDSPxDPDhjNRU2wE="; } {};

      streamly-core = self.callHackageDirect {
      		    pkg = "streamly-core";
		    ver = "0.2.0";
  		    sha256 = "sha256-fMo5dz/AY0CUZaP1lhXqjfsuGVO4GtAW3/q9W9N6D3Q="; } {};

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
