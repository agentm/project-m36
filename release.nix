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

       curryer-rpc =
         self.callHackageDirect {
           pkg = "curryer-rpc";
	   ver = "0.1";
	   sha256 = "085j4yjr8wimxampxqdrg47p577whwg2mjxgfdl44yfck52fb6ac";
       }{};

       winery =
         self.callHackageDirect {
	   pkg = "winery";
	   ver = "1.3.2";
	   sha256 = "1jc7q8m0zd8bziwa3mra44j518ckc566akk7r5hqpf8x5ng0n4vv";
       }{};

       barbies-th =
         self.callHackageDirect {
           pkg = "barbies-th";
	   ver = "0.1.7";
	   sha256 = "0gf39kcn6lwqhz888qm3iypma76yrp3zwmif1niry647qq27xp4j";
       }{};

       barbies =
         self.callHackageDirect {
           pkg = "barbies";
	   ver = "2.0.2.0";
	   sha256 = "0753rd8pw0x1cgasz8a6633kg83jngwzncp9khbimllvcdqzardh";
       }{};

       prettyprinter =
         self.callHackageDirect {
           pkg = "prettyprinter";
	   ver = "1.7.0";
	   sha256 = "17byy08brwcsl5rqdhibq3pcpgx085shizb2ap6s4xy3izdia3cc";
	 }{};

       prettyprinter-ansi-terminal =
         self.callHackageDirect {
           pkg = "prettyprinter-ansi-terminal";
	   ver = "1.1.2";
	   sha256 = "0lwcqndppw3jc55rlnn6sp76zmjx2yzl21g9jhg27k2rdnjwd7md";
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