let
  # Nixpkgs snapshot.
  sources = import ./package/nix/sources.nix;
  # The final "pkgs" attribute with all the bells and whistles of our overlays.
  pkgs = import sources.nixpkgs {};
in

# This is our development shell.
pkgs.mkShell ({
  buildInputs = [
    # Tangling and weaving for Literate Programming.
    pkgs.emacs

    # Misc
    pkgs.git
    pkgs.less
  ];
})
