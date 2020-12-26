{ compiler ? "ghc865"
, pkgs ? import ./pkgs.nix
}:

let
  inherit (pkgs.lib.trivial) flip pipe;
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags;
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  dontHaddock = pkgs.haskell.lib.dontHaddock;
  dontCheck = pkgs.haskell.lib.dontCheck;
  distProcessPkg = pkg : dontHaddock (doJailbreak (dontCheck pkg));
  callHackageDirectWithOptions = {pkg,ver, sha256, options}:
  let pkgver = "${pkg}-${ver}";
  in
          haskellPackages.callCabal2nixWithOptions pkg (pkgs.fetchzip {
	  url = "mirror://hackage/${pkgver}/${pkgver}.tar.gz";
	  inherit sha256;
	  }) options;
	
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
       rank1dynamic =
	 distProcessPkg (self.callHackageDirect {
	   pkg = "rank1dynamic";
  	   ver = "0.4.0";
	   sha256 = "1x6p9z53s7xwp93jp4n1lq6m5xspcn6w62p461v8vqnp9600328b";
}{});
       stm-hamt =
         self.callHackageDirect {
	   pkg = "stm-hamt";
	   ver = "1.2.0.3";
	   sha256 = "1i8j7q8nhx3l7gydrjlsyhkvllr6bppgrjf0hp38zpm7p6xcv2bb";
	   }{};

        stm-containers =
	  self.callHackageDirect {
	    pkg = "stm-containers";
	    ver = "1.1.0.4";
	    sha256 = "0h9z6lawdm44minx9spvdb7sp645l36ri5cnxvhrcxwzr64s7qb5";
	  }{};

        data-interval =
          self.callHackageDirect {
             pkg = "data-interval";
	     ver = "2.0.1";
	     sha256 = "085j4yjr8wimxampxqdrg47p577whwg2mjxgfdl44yfck52fb6an";
          }{};

       curryer =
         self.callHackageDirect {
           pkg = "curryer";
	   ver = "0.1";
	   sha256 = "085j4yjr8wimxampxqdrg47p577whwg2mjxgfdl44yfck52fb6ac";
       }{};
      
      project-m36 = dontCheck (self.callCabal2nixWithOptions "project-m36" ./. "-f-haskell-scripting" { });

    };
  };

  project = haskellPackages.project-m36;
in
{
  project = project;

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      project
    ];
    buildInputs = [
      haskellPackages.ghcid
      haskellPackages.hlint
      pkgs.docker
    ];
    withHoogle = true;
  };
}