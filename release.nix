{ compiler ? "ghc865"
, pkgs ? import ./pkgs.nix
}:

let
  inherit (pkgs.lib.trivial) flip pipe;
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags;
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  dontHaddock = pkgs.haskell.lib.dontHaddock;
  dontCheck = pkgs.haskell.lib.dontCheck;
  #distributed-process packages are no longer maintained- until we replace it entirely, add some hacks to build on newer GHC and packages
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
      distributed-process-extras =
        distProcessPkg (self.callHackageDirect {
	  pkg = "distributed-process-extras";
	  ver = "0.3.5";
	  sha256 = "13clq3alymcvknp7q8gz0i1hzx8im855lzmyrfffl8gglnlbi2w9";
	  }{});

       distributed-process-client-server =
        distProcessPkg (self.callHackageDirect {
		pkg = "distributed-process-client-server";
		ver = "0.2.5.1";
		sha256 = "0gkd8awapq7vy14vpnzawnwb67jvvcjaqg7imm0bp1wb6rw6x5x4";
		}{});
		
       distributed-process-async =
         distProcessPkg (callHackageDirectWithOptions {
	        pkg = "distributed-process-async";
		ver = "0.2.6";
		sha256 = "15wyfq115r6xjaz1h00sjh1v0fv498kkxr3xx6w84d8wdp2gxbn1";		
		options = "--no-check";
		}{});

       distributed-process =
	        dontHaddock (self.callCabal2nix "distributed-process" (pkgs.fetchFromGitHub {
		owner = "agentm";
		repo = "distributed-process";
		rev = "f352e097fbe494ad648b4e09249f865893ae90a3";
		sha256 = "0xa2v77c9idmi89bky2wq1fykrn8grpk4rk7gcd5bkicxvdbcrym";}){});

       distributed-static =
         self.callHackageDirect {
	   pkg = "distributed-static";
	   ver = "0.3.9";
	   sha256 = "0hibpzqwklnq335m7fwgk1bghz0lgxnd7ym10pbvxlcf7ly3nr6v";
}{};
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

        network-transport-tcp =
          dontCheck (self.callHackageDirect {
		  pkg = "network-transport-tcp";
		  ver = "0.7.0";
		  sha256 = "0kfzrcynf34xsxx4rccg4qfy7jvinwqd8baian8ifqad9lfcapsb";
		  }{});

        data-interval =
          self.callHackageDirect {
             pkg = "data-interval";
	     ver = "2.0.1";
	     sha256 = "085j4yjr8wimxampxqdrg47p577whwg2mjxgfdl44yfck52fb6an";
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