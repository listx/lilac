{ sources, compiler, }: [
  (final: prev: {
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreFilter;
  })
  (final: prev: {
    lilac = import ./packages.nix {
      pkgs = prev;
      inherit compiler;
    };
  })
]
