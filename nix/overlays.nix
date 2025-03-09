{ sources, compiler, lilac_git_version }: [
  (final: prev: {
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreFilter;
  })
  (final: prev: {
    lilac = import ./packages.nix {
      pkgs = prev;
      inherit compiler;
      inherit lilac_git_version;
    };
  })
]
