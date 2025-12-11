{ lilacGitVersion ? "unknown" }:
let sources = import ./sources.nix; # (ref:import sources.nix)
in import sources.nixpkgs {
  system = builtins.currentSystem;
  config = { };
  overlays = [
    (final: prev: {
      lilacPkgs = import ./packages.nix { # (ref:import packages.nix)
        pkgs = prev;
        fetchFromGitHub = prev.fetchFromGitHub;
        inherit lilacGitVersion;
      };
    })
  ];
}
