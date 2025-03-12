{ lilacGitVersion ? "unknown" }:
let pkgs = import ./default.nix { inherit lilacGitVersion; };
in pkgs.lilacPkgs.lilac
