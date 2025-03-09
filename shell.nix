{ system ? builtins.currentSystem, compiler ? null, }:
let pkgs = import ./nix { inherit system compiler; };
in pkgs.mkShell {
  buildInputs = [ pkgs.lilac.shell ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lilac.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
