{ pkgs, lilacGitVersion }:
let
  devShellEnv = pkgs.buildEnv {
    name = "lilac-devShellEnv";
    paths = scripts ++ tools ++ [ ghc ];
  };
  scripts = import ./scripts.nix { inherit pkgs; };
  tools = [
    pkgs.niv
    pkgs.cabal-install
    pkgs.editorconfig-checker
    pkgs.shellcheck
    pkgs.shfmt
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
        hlib.disableExecutableProfiling
        hlib.disableLibraryProfiling
      ];

    in lib.pipe haskellPkg confFns;
  gitHooksPkgs = import (import ./sources.nix)."git-hooks.nix";
  pre-commit-check = gitHooksPkgs.run {
    src = projectRoot;
    hooks = {
      editorconfig-checker = {
        enable = true;
        package = devShellEnv;
      };
      nixfmt-rfc-style = {
        enable = true;
        package = devShellEnv;
        excludes = [ "nix/sources.nix" ];
      };
      ormolu = {
        enable = true;
        package = devShellEnv;
        entry = let opts = [ "--mode" "inplace" "--check-idempotence" ];
        in "${devShellEnv}/bin/ormolu ${toolOpts opts}";
      };
      shfmt = {
        enable = true;
        package = devShellEnv;
        entry = let
          opts = [
            "--indent"
            "0"
            "--binary-next-line"
            "--func-next-line"
            "--write"
          ];
        in "${devShellEnv}/bin/shfmt ${toolOpts opts}";
      };
      shellcheck = {
        enable = true;
        package = devShellEnv;
      };
      check-tangled-vs-tracked = {
        name = "check-tangled-vs-tracked.sh";
        enable = true;
        always_run = true;
        entry = "./check-tangled-vs-tracked.sh";
        pass_filenames = false;
      };
      unit-test = {
        name = "unit-test.sh";
        enable = true;
        always_run = true;
        entry = "./unit-test.sh";
        pass_filenames = false;
        stages = [ "pre-push" ];
      };
    };
  };

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
