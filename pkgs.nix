let
nixpkgs = builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  rev = "f58482658eabf7d695f45904ecbc5f49c19fef98";
};
in
import nixpkgs { config.allowBroken = true; }