{ compiler ? "ghc928"
, sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  release = import ./release.nix { inherit pkgs compiler sources; };
  static-project-m36 = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.justStaticExecutables release.project);
in
pkgs.dockerTools.buildImage {
  name = "project-m36";
  tag = "latest";
  contents = [ static-project-m36 ];
  # expose default project-m36 and websocket server ports
  config = {
    Env = [ "LC_ALL=en_US.UTF-8" ];
    ExposedPorts = { "6543/tcp" = { }; "8000/tcp" = { }; };
  };
}
