{ pkgs ? import ./pkgs.nix }:
let
  release = import ./release.nix { inherit pkgs; };
  static-project-m36 = pkgs.haskell.lib.justStaticExecutables release.project;
in
 pkgs.dockerTools.buildImage {
   name = "project-m36";
   tag = "latest";
   contents = [static-project-m36];
   config = { Env = ["LC_ALL=en_US.UTF-8"]; };
 }