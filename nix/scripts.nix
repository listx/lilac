{pkgs}: let
  logo = pkgs.writeShellScriptBin "logo" ''
    set -euo pipefail
    echo -e "\n$(tput setaf 4)"
    echo Lilac | ${pkgs.figlet}/bin/figlet
    echo -e "$(tput sgr0)\n"
  '';
  build = pkgs.writeShellScriptBin "build" ''
    set -euo pipefail
    nix-build nix/release.nix
  '';
  lilac = pkgs.writeShellScriptBin "lilac" ''
    set -euo pipefail
    result/bin/lilac "$@"
  '';
in [logo build lilac]
