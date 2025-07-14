{ pkgs }:
let
  logo = pkgs.writeShellScriptBin "logo" ''
    set -euo pipefail
    echo -e "\n$(tput setaf 4)"
    echo Lilac | ${pkgs.figlet}/bin/figlet
    echo -e "$(tput sgr0)\n"
  '';
  lilac = pkgs.writeShellScriptBin "lilac" ''
    set -euo pipefail
    cabal run lilac -v0 -- "$@"
  '';
  lilac-nix = pkgs.writeShellScriptBin "lilac-nix" ''
    set -euo pipefail
    result/bin/lilac "$@"
  '';
in [ logo lilac lilac-nix ]
