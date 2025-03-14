{ pkgs, lilacGitVersion }:
let
  devShellEnv = pkgs.buildEnv {
    name = "lilac-devShellEnv";
    paths = scripts ++ tools ++ [ ghc ];
  };

  scripts = import ./scripts.nix { inherit pkgs; };

  tools = [
    pkgs.cabal-install
    pkgs.niv
    pkgs.shellcheck
    pkgs.shfmt
    pkgs.treefmt
    ourHaskell.nixfmt
    ourHaskell.ormolu
  ];

  hlib = pkgs.haskell.lib;
  ghc = ourHaskell.ghc.withPackages
    (_: hlib.getHaskellBuildInputs ourHaskell.lilac);

  lib = pkgs.lib;
  ourHaskell = pkgs.haskell.packages.${ghcVersion}.extend
    (lib.composeManyExtensions haskellOverlays);

  ghcVersionWithoutDots = lib.replaceStrings [ "." ] [ "" ] "9.10.1";
  ghcVersion = "ghc" + ghcVersionWithoutDots;

  haskellOverlays = [ injectLilac tweakLilacBuild ];

  injectLilac = final: prev: {
    lilac = configureHaskellPkg (prev.callCabal2nix "lilac" lilacSource { });
  };

  tweakLilacBuild = final: prev: {
    lilac = prev.lilac.overrideAttrs (drv: {
      buildPhase = ''
        export LILAC_GIT_VERSION=${lilacGitVersion}
        echo "Using LILAC_GIT_VERSION $LILAC_GIT_VERSION"
      '' + drv.buildPhase;
    });
  };

  projectRoot = ../.;

  gnf = import (import ./sources.nix)."gitignore.nix" { inherit lib; };
  lilacSource = lib.cleanSourceWith {
    src = projectRoot;
    name = baseNameOf projectRoot + "-src";
    filter = let
      filterFunc = gnf.gitignoreFilterWith {
        basePath = projectRoot;
        extraRules = ''
          .*
          ,,*.html
          ,,*.nix
          ,,*.org
          ,,*.sh
          ,,*.toml
          Makefile
          image
          nix
        '';
      };
    in path: type: filterFunc path type;
  };

  configureHaskellPkg = haskellPkg:
    let
      confFns = [
        hlib.dontHyperlinkSource
        hlib.dontCoverage
        hlib.dontHaddock
        (hlib.compose.appendConfigureFlags [
          "--ghc-option=+RTS"
          "--ghc-option=-A256m"
          "--ghc-option=-n2m"
          "--ghc-option=-RTS"
        ])
      ]

        ++ lib.optional false hlib.disableOptimization

        ++ lib.optional false hlib.enableExecutableProfiling

        ++ lib.optional false hlib.doBenchmark

        ++ lib.optional pkgs.stdenv.isAarch64 (hlib.compose.appendConfigureFlag
          "--ghc-option=-fwhole-archive-hs-libs");

    in lib.pipe haskellPkg confFns;

  gitHooksPkgs = import (import ./sources.nix)."git-hooks.nix";
  pre-commit-check = gitHooksPkgs.run {
    src = projectRoot;
    hooks = {
      nixfmt-rfc-style = {
        enable = true;
        package = devShellEnv;
        excludes = [ "nix/sources.nix" ];
      };

      ormolu = {
        enable = true;
        package = devShellEnv;
        entry = "${devShellEnv}/bin/ormolu ${
            toolOpts treefmtConf.formatter.haskell.options
          }";
      };

      shfmt = {
        enable = true;
        package = devShellEnv;
        entry = "${devShellEnv}/bin/shfmt ${
            toolOpts treefmtConf.formatter.shell.options
          }";
      };

      shellcheck = {
        enable = true;
        package = devShellEnv;
      };
    };
  };

  treefmtConf = lib.importTOML ../treefmt.toml;
  toolOpts = opts: builtins.concatStringsSep " " opts;
in {
  # Derivation for the shell environment. This is fed into "pkgs.mkShell" to
  # give us the final shell environment.
  devShell = pkgs.mkShell {
    buildInputs = [ devShellEnv ];
    shellHook = ''
      export LD_LIBRARY_PATH=${devShellEnv}/lib:$LD_LIBRARY_PATH
      # Used when building with cabal.
      export LILAC_PROJECT_ROOT=$PWD
      # Set up Git pre-commit hooks.
      ${pre-commit-check.shellHook}
      logo
    '';
  };

  lilac = ourHaskell.lilac;
}
