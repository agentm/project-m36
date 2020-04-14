{ compiler ? "ghc865" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              net-mqtt =
                haskellPackagesNew.callPackage ./nix/net-mqtt.nix { };

              ptm-common =
                haskellPackagesNew.callPackage ./common/common.nix { };

              ptm-core =
                haskellPackagesNew.callPackage ./core/core.nix { };

              ptm-api =
                haskellPackagesNew.callPackage ./API/api.nix { };

              transient =
                haskellPackagesNew.callPackage ./nix/transient.nix { };

              TCache =
                haskellPackagesNew.callPackage ./nix/TCache.nix { };

              transient-universe =
                haskellPackagesNew.callPackage ./nix/transient-universe.nix { };

              servant-auth-token =
                haskellPackagesNew.callPackage ./nix/servant-auth-token.nix { };

              aeson-injector =
                haskellPackagesNew.callPackage ./nix/aeson-injector.nix { };

              servant-auth-token-api =
                haskellPackagesNew.callPackage ./nix/servant-auth-token-api.nix { };

              servant-auth-token-persistent =
                haskellPackagesNew.callPackage ./nix/servant-auth-token-persistent.nix { };

            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { ptm-api = pkgs.haskell.packages.${compiler}.ptm-api;
    ptm-core = pkgs.haskell.packages.${compiler}.ptm-core;
    ptm-common = pkgs.haskell.packages.${compiler}.ptm-common;
    net-mqtt = pkgs.haskell.packages.${compiler}.net-mqtt;
    transient-universe = pkgs.haskell.packages.${compiler}.transient-universe;
    transient = pkgs.haskell.packages.${compiler}.transient;
    TCache = pkgs.haskell.packages.${compiler}.TCache;
    servant-auth-token = pkgs.haskell.packages.${compiler}.servant-auth-token;
    servant-auth-token-api = pkgs.haskell.packages.${compiler}.servant-auth-token-api;
    servant-auth-token-persistent = pkgs.haskell.packages.${compiler}.servant-auth-token-persistent;
    aeson-injector = pkgs.haskell.packages.${compiler}.aeson-injector;
  }
