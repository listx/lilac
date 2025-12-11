{ lilacGitVersion ? "unknown" }:
let
  pkgs = import ./default.nix { inherit lilacGitVersion; }; # (ref:default.nix)
in pkgs.lilacPkgs.lilac
