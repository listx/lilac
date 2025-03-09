{ pkgs, lib, gitignoreFilter, }: {
  /* Given a package return its derivation

     > getDrv "haskellPackages.implicit-hie"
     «derivation /nix/store/qb9j7iccna81dcs1szwryl4izli95w53-implicit-hie-0.1.2.7.drv»
  */
  getDrv = path: lib.getAttrFromPath (lib.splitString "." path) pkgs;

  # Filter using .gitignore + ignore files and paths defined in the config
  filterSrc =
    { path, name ? (baseNameOf path + "-src"), files ? [ ], paths ? [ ], }:
    lib.cleanSourceWith {
      src = path;
      inherit name;
      filter = let
        gitIgnore = gitignoreFilter path; # in let binding to memoize
        relToPath = lib.removePrefix (toString path + "/");
      in path: type:
      gitIgnore path type && !builtins.elem (baseNameOf path) files
      && !lib.any (d: lib.hasPrefix d (relToPath path)) paths;
    };
}
