{ compiler ? "ghc967"
, sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  release = import ./release.nix {
    inherit pkgs compiler sources;
  };

  # Ensure project returns a valid derivation
  project = pkgs.haskell.lib.dontCheck release.project;

  # Fetch GHC from the Haskell packages
   ghc = pkgs.haskell.packages.${compiler}.ghc; #.override (drv: { enableDocs = false; }); #altering this package requires rebuilding it, but I just want to remove ghc-9.6.7-docs
   ghcWithPackages = release.haskellDeps.ghcWithPackages (p: [p.project-m36]);

  # Specify the bin and lib paths explicitly
  projectBin = "${project}/bin";
  ghcBin = "${ghc}/bin";
  ghcLib = "${ghc}/lib";
  projectLib = "${project}/lib";
  glibcLib = "${pkgs.glibc}/lib";
  ghcWithPackagesDatabase = "${ghcWithPackages}/lib/ghc-9.6.7/lib/package.conf.d";
in
pkgs.dockerTools.buildImage {
  name = "project-m36";
  tag = "latest";
  copyToRoot = [
    ghc
    ghcWithPackages
    project
    pkgs.bash
    pkgs.coreutils
    pkgs.glibc
#    pkgs.glibcLocales
  ];
  config = {
    Env = [
#      "LC_ALL=en_US.UTF-8"
      "LANG=en_US.UTF-8" 
      "PATH=/bin:/usr/bin:${projectBin}:${ghcBin}"
      "LD_LIBRARY_PATH=${ghcLib}:${projectLib}:${glibcLib}"
      "GHC_PACKAGE_PATH=${ghcWithPackagesDatabase}"
    ];
    ExposedPorts = { "6543/tcp" = { }; "8000/tcp" = { }; };
    Cmd = [ "/bin/sh" "-c" "export PATH=$PATH:${projectBin}:${ghcBin} && exec tutd" ];
  };
}
