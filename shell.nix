let
  # Nixpkgs snapshot.
  sources = import ./package/nix/sources.nix;
  # The final "pkgs" attribute.
  pkgs = import sources.nixpkgs {};
in

# This is our development shell.
pkgs.mkShell ({
  buildInputs = [
    # Tangling and weaving for Literate Programming.
    pkgs.emacs

    # Update Nix dependencies in package/nix/sources.nix.
    pkgs.niv

    # Misc.
    pkgs.git
    pkgs.less
  ];
})
