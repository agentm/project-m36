{ compiler ? "ghc967"
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
    	      mkDerivation = args: super.mkDerivation (args // {
              		   doCheck = false;
            	      });
      asn1-types = self.callHackageDirect {
        	      pkg = "asn1-types";
		      ver = "0.3.4";
                      sha256 = "sha256-8IM10LMTzvBUnHkYMpGkFVMo+iYOMXse0FgrtgBeFO8="; } {};

      attoparsec = doJailbreak super.attoparsec; #for QuickCheck 2.16
		      
      crypton = self.callHackageDirect {
                  pkg = "crypton";
		  ver = "1.0.4";
                  sha256 = "sha256-2NHVnfhkxchdspnQNDEFk8Pdsd6hNy5GEt28LgyT008="; } {};
		  
      crypton-x509-validation = self.callHackageDirect {
                  pkg = "crypton-x509-validation";
	          ver = "1.8.0";
                  sha256 = "sha256-i/KEYFBJHwEegsH5LsTrfHulljC50xnVd9xqOXsXRvw="; } {};
		  
      crypton-x509-system = self.callHackageDirect {
        	      pkg = "crypton-x509-system";
		      ver = "1.8.0";
                      sha256 = "sha256-y/xeY8CES/KuBRKsoT0Ldjh0EdWTRNGCldU6nYQi/Ug="; } {};
		      
      crypton-x509 = self.callHackageDirect {
        	      pkg = "crypton-x509";
		      ver = "1.8.0";
                      sha256 = "sha256-wxU8Ou52UCuCT2gbxqPKssteIVGUyg5WEbv1xRIyZTg="; } {};
		      
      crypton-asn1-encoding = self.callHackageDirect {
      			    pkg = "crypton-asn1-encoding";
			    ver = "0.10.0";
                            sha256 = "sha256-dTP26qiOVnAb5XO/gibuG1rYI03vDTpBr6+L79PsjEA="; } {};
			    
      crypton-asn1-parse = self.callHackageDirect {
      			   pkg = "crypton-asn1-parse";
			   ver = "0.10.0";
                           sha256 = "sha256-awY6Rk3LBgOZk9xEKGlPWtYYD8OiKAskN26otBbVXjc="; } {};			   

      crypton-pem = self.callHackageDirect {
      		           pkg = "crypton-pem";
			   ver = "0.3.0";
                           sha256 = "sha256-RBQdPqN/UJw+9FU/HAh5wR5S69WOfllnWAs/mpegbK8="; } {};			   			   
      
      curryer-rpc = self.callHackageDirect {
                      pkg = "curryer-rpc";
		      ver = "0.5.1";
		      sha256 = "sha256-4a6iNNTvMwox4RzdeLue2tyysEXosu9aNWCXCQLNyWc="; } {};

      ech-config = self.callHackageDirect {
      		    pkg = "ech-config";
		    ver = "0.0.1";
		    sha256 = "sha256-Qg8K5Le7EiRYHPfo5NBwzEqYNGJRJPgqCX0wmlPrvWs=";
      } {};

      hashable = self.callHackageDirect {
      		    pkg = "hashable";
		    ver = "1.4.7.0";
		    sha256 = "sha256-ykQ0pQ2SIUPnkESkuBNBRT7kwtl7pLYKJ+s0OYei0/0=";
      } {};

      http-api-data = doJailbreak super.http-api-data;

      http-client-tls = self.callHackageDirect {
      		    pkg = "http-client-tls";
		    ver = "0.3.6.4";
		    sha256 = "sha256-70vp+ERuG6e5jjx/qq/yoVuzwyKF17udvtzPQknKC7k=";
      } {};

      indexed-traversable-instances = doJailbreak super.indexed-traversable-instances;

      integer-conversion = doJailbreak (self.callHackageDirect {
      		    pkg = "integer-conversion";
		    ver = "0.1.1";
		    sha256 = "sha256-I/vk2uj0KZSwBoJQq07+SOZVGS+RfxK0wQ4g1oeBLEs=";
      } {});

      hpke = self.callHackageDirect {
      	       pkg = "hpke";
	       ver = "0.0.0";
               sha256 = "sha256-KWvfDLcJEm+v5LbbmrSqz6GDh34PIVCOBjMjjl/x1m8=";
      } {};

       integer-logarithms = doJailbreak (self.callHackageDirect {
      	       pkg = "integer-logarithms";
	       ver = "1.0.4";
               sha256 = "sha256-c/Sr3ysk9EWktoR5B1wzI7F/aRGuapLnlDfU2csD0ns=";
      } {});

      network = doJailbreak (self.callHackageDirect {
      	       pkg = "network";
	       ver = "3.2.8.0";
               sha256 = "sha256-h9dulFUDB3lGzZqPgdX+WBGzRdncqBDLVKUb//hw1n8=";
      } {});

      optparse-applicative = doJailbreak (self.callHackageDirect {
      	       pkg = "optparse-applicative";
	       ver = "0.19.0.0";
               sha256 = "sha256-dhqvRILfdbpYPMxC+WpAyO0KUfq2nLopGk1NdSN2SDM=";
      } {});

      os-string = doJailbreak (self.callHackageDirect {
      	       pkg = "os-string";
	       ver = "2.0.8";
               sha256 = "sha256-5SenenQzGstqSyK/QHppV2XupPmCQ+8ditlzofCreMQ=";
      } {});

      psqueues = doJailbreak (self.callHackageDirect {
      	       pkg = "psqueues";
	       ver = "0.2.8.2";
               sha256 = "sha256-RsToGunr5/RdyO/IKkDFtpI/oqVYJ4kMp0xOAUZMHS8=";
      } {});
      
      QuickCheck = self.callHackageDirect {
      	       pkg = "QuickCheck";
	       ver = "2.16.0.0";
               sha256 = "sha256-HWL+HFdHG6Vnk+Thfa0AoEjgPggiQmyKMLRYUUKLAZU=";
      } {};

      streamly = doJailbreak (self.callHackageDirect {
                   pkg = "streamly";
		   ver = "0.10.1";
		   sha256 = "sha256-9tWZ/8YteD9ljhEmj8oYKIAyFcbQflX0D20j/NTe3qM="; } {});

      streamly-core = self.callHackageDirect {
      		    pkg = "streamly-core";
		    ver = "0.2.2";
  		    sha256 = "sha256-Ggo5ius3dp/TJFfrZSk31A6gSZHA6kLMtxFKe9MIvqQ="; } {};
		    
      streamly-bytestring = self.callHackageDirect {
                    pkg = "streamly-bytestring";
		    ver = "0.2.3";
  		    sha256 = "sha256-ZBV7RO6ibwNKA8S/zr2r31YTQYk4vrP5d7dieTC71hY="; } {};

      text-iso8601 = doJailbreak super.text-iso8601;
      time-compat = doJailbreak super.time-compat;

      lockfree-queue = self.callHackageDirect {
      		     pkg = "lockfree-queue";
		     ver = "0.2.4";
                     sha256 = "sha256-h1s/tiBq5Gzl8FtenQacmxJp7zPJPnmZXtKDPvxTSa4="; } {};
      

      unicode-data = self.callHackageDirect {
                      pkg = "unicode-data";
		      ver = "0.2.0";
		      sha256 = "14crb68g79yyw87fgh49z2fn4glqx0zr53v6mapihaxzkikhkkc3";
		     } {};

      uuid-types = doJailbreak super.uuid-types;
      uuid = doJailbreak super.uuid;
      universe-base = doJailbreak super.universe-base;

      barbies-th = doJailbreak (self.callHackageDirect {
      	      pkg = "barbies-th";
	      ver = "0.1.11";
  	      sha256 = "sha256-U9mHuHAA0v74dKB2w2kLGx9dBKU6w8CRObtYQF97Gao=";	      
      } {});

      barbies = doJailbreak (self.callHackageDirect {
      	      pkg = "barbies";
	      ver = "2.0.5.0";
  	      sha256 = "sha256-kivybLAtSG9sVfUtqsPleq5DIiSe+Bwm9YuwuumCOJM=";
      } {});

      random = self.callHackageDirect {
      	      pkg = "random";
	      ver = "1.3.1";
  	      sha256 = "sha256-M2xVhHMZ7Lqvx/F832mGirHULgzv7TjP/oNkQ4V6YLM=";
      } {};
      
      scotty = doJailbreak (self.callHackageDirect {
      	     pkg = "scotty";
	     ver = "0.22";
             sha256 = "sha256-DY4lKmAmqGTrzKq93Mft9bu9Qc0QcsEVpKzgoWcBL2I=";
	     } {});

      tasty-quickcheck = doJailbreak (self.callHackageDirect {
      	     pkg = "tasty-quickcheck";
	     ver = "0.10.3";
             sha256 = "sha256-lxO3ZoJ/RtA9jxDq7TBgGfZUXmbQJYE8ws64sdkPQhw=";
	     } {});

      tls = self.callHackageDirect {
      	     pkg = "tls";
	     ver = "2.2.0";
             sha256 = "sha256-0p5d8QuIMsTwru8PIkns4vdc4zD5PyubzWxOaBJ89IM=";
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

      winery = self.callHackageDirect {
           pkg = "winery";
	   ver = "1.5";
           sha256 = "sha256-eI6D+lVG+3ru6Jt55wpcaHlmNydT3VnOtBo3NhKs16k=";
	   } {};

      linux-xattr = doJailbreak (self.callHackageDirect {
          pkg = "linux-xattr";
	  ver = "0.1.1.0";
         sha256 = "sha256-l35drTPGKwpip77/3BwDr7Eo0Arjfp34Cc3oRGyf+po=";
      } {});

      project-m36 = ((self.callCabal2nixWithOptions "project-m36" ./. "-f-haskell-scripting" { mkDerivation = args: self.mkDerivation (args // {
      		    			doCheck = true;
					});
  }));
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
