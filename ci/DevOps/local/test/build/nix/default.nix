{ compiler ? "ghc865" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              /*primitive-unlifted =
                haskellPackagesNew.callPackage ./primitive-unlifted.nix { };*/
              distributed-process-extras =
                haskellPackagesNew.callPackage ./distributed-process-extras.nix { };
              distributed-process-client-server =
                haskellPackagesNew.callPackage ./distributed-process-client-server.nix { };
              distributed-process-async =
                haskellPackagesNew.callPackage ./distributed-process-async.nix { };
              rank1dynamic =
                haskellPackagesNew.callPackage ./rank1dynamic.nix { };
              distributed-static =
                haskellPackagesNew.callPackage ./distributed-static.nix { };
              distributed-process =
                haskellPackagesNew.callPackage ./distributed-process.nix { };
              stm-hamt =
                haskellPackagesNew.callPackage ./stm-hamt.nix { };
              stm-containers =
                haskellPackagesNew.callPackage ./stm-containers.nix { };
              network-transport-tcp =
                haskellPackagesNew.callPackage ./network-transport-tcp.nix { };
              data-interval =
                haskellPackagesNew.callPackage ./data-interval.nix { };
              project-m36 =
                haskellPackagesNew.callPackage ./project-m36.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  inherit (pkgs) dockerTools stdenv buildEnv writeText;

  project-m36 = pkgs.haskell.packages.${compiler}.project-m36;

  static-project-m36 = pkgs.haskell.lib.justStaticExecutables pkgs.haskell.packages.${compiler}.project-m36;

  passwd = ''
    root:x:0:0::/root:/run/current-system/sw/bin/bash
    project-m36:x:90001:90001::/var/empty:/run/current-system/sw/bin/nologin
  '';

  group = ''
    root:x:0:
    nogroup:x:65534:
    project-m36:x:90001:project-m36
  '';

  nsswitch = ''
    hosts: files dns myhostname mymachines
  '';

  project-m36-conf = ''
    para1 = "$(PARA1)"
    para2 = "$(PARA2)"
  '';

  project-m36-env = stdenv.mkDerivation {
    name = "project-m36-env";
    phases = [ "installPhase" "fixupPhase" ];

    installPhase = ''
      mkdir -p $out/etc/project-m36
      echo '${project-m36-conf}' > $out/etc/project-m36/project-m36.conf
      echo '${passwd}' > $out/etc/passwd
      echo '${group}' > $out/etc/group
      echo '${nsswitch}' > $out/etc/nsswitch.conf
    '';
  };

  project-m36-docker =  pkgs.dockerTools.buildImage {
  name = "project-m36";
  tag = project-m36.version;
  
  contents = [ static-project-m36
               project-m36-env ];
  config = {
    Env = [ 
    "PARA1="
    "PARA2="
    ];
    User = "project-m36";
    Cmd = [ "${static-project-m36}/bin/project-m36" "/etc/project-m36/project-m36.conf" ];
    ExposedPorts = {
      "5432/tcp" = {};
    };
    WorkingDir = "/data";
    Volumes = {
      "/data" = {};
    };
  };
};
in  {
  inherit project-m36;
  inherit static-project-m36;
  inherit project-m36-docker;
}
