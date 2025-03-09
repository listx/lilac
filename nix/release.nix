{ system ? builtins.currentSystem, compiler ? null
, lilac_git_version ? "unknown" }:
let pkgs = import ./. { inherit system compiler lilac_git_version; };
in pkgs.lilac
