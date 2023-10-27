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
    pkgs.emacs29-nox

    # For evaluation of Python source code blocks.
    pkgs.python3Minimal

    # Spell checking.
    pkgs.typos

    # Update Nix dependencies in package/nix/sources.nix.
    pkgs.niv

    # Misc.
    pkgs.git
    pkgs.less
  ];
})
