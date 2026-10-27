{ lilacGitVersion ? null, rev ? null }:
let
  version = if lilacGitVersion != null then
    lilacGitVersion
  else if rev != null then
    import ./version.nix {
      cabalFile = ../lilac.cabal;
      inherit rev;
    }
  else
    "unknown";
  pkgs =
    import ./default.nix { lilacGitVersion = version; }; # (ref:default.nix)
in pkgs.lilacPkgs.lilac
