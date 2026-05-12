{ pkgs, fetchFromGitHub, lilacGitVersion }:
let
  devShellEnv = pkgs.buildEnv {
    name = "lilac-devShellEnv";
    paths = scripts ++ tools ++ [ ghc ];
  };

  scripts = import ./scripts.nix { inherit pkgs; };

  tools = [
    pkgs.clojure

    pkgs.niv

    pkgs.cabal-install
    #
    ourHaskell.hspec-discover
    pkgs.editorconfig-checker

    pkgs.shellcheck

    pkgs.shfmt

    ourHaskell.nixfmt

    ourHaskell.ormolu

    pkgs.parinfer-rust

    pkgs.vale
  ];

  hlib = pkgs.haskell.lib;
  ghc = ourHaskell.ghc.withPackages
    (_: hlib.getHaskellBuildInputs ourHaskell.lilac);

  lib = pkgs.lib;

  #
  ourHaskell = pkgs.haskell.packages.${ghcVersion}.extend
    (lib.composeManyExtensions haskellOverlays);

  ghcVersionWithoutDots = lib.replaceStrings [ "." ] [ "" ] "9.12.2";
  ghcVersion = "ghc" + ghcVersionWithoutDots;

  haskellOverlays =
    [ heavyDeps jailbreaks injectExternalPackages injectLilac tweakLilacBuild ];

  heavyDeps = final: prev:
    let
      heavyPackages = [
        "brick"
        "fsnotify"
        "pandoc-types"
        "sandwich"
        "skylighting"
        "skylighting-core"
        "skylighting-format-ansi"
        "skylighting-format-blaze-html"
        "skylighting-format-latex"
        "th-lift-instances"
        "unicode-collation"
        "unicode-transforms"
        "vty-crossplatform"
        "wai"
        "wai-app-static"
        "wai-extra"
        "wai-websockets"
        "warp"
        "websockets"
      ];
    in lib.genAttrs heavyPackages (name: buildForCI prev.${name});

  jailbreaks = final: prev: {
    unicode-data = pkgs.haskell.lib.dontCheck prev.unicode-data;
  };

  injectExternalPackages = final: prev: {
    citeproc = buildFaster (prev.callCabal2nix "citeproc" (let
      repo = fetchFromGitHub {
        owner = "jgm";
        repo = "citeproc";
        rev = "e5f22f0b0f7662524ff52883582d883316f4c113";
        sha256 = "sha256-xdwUdmmiTwHn0o0BeX949FPQFftHiTUmzieijpLxruc=";
      };
    in "${repo}") { });
  };

  injectLilac = final: prev: { # (ref:injectLilac)
    lilac = buildFaster (prev.callCabal2nix "lilac" lilacSource { });
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

  gnf =
    import (import ./sources.nix)."gitignore.nix" { inherit lib; }; # (ref:gnf)
  lilacSource = lib.cleanSourceWith {
    src = projectRoot;
    name = baseNameOf projectRoot + "-src"; # (ref:lilacSource name)
    filter = let # (ref:gnf-memoization)
      filterFunc = gnf.gitignoreFilterWith {
        basePath = projectRoot;
        # (ref:nix:lilacSource-extraRules)
        extraRules = ''
          .*
          *.html
          *.nix
          *.sh
          *.toml
          image
          Makefile
          nix
        '';
      };
    in path: type: filterFunc path type;
  };

  buildForCI = haskellPkg:
    let
      confFns = [
        hlib.dontHyperlinkSource
        hlib.dontCoverage
        hlib.dontHaddock
        hlib.disableExecutableProfiling
        hlib.dontCheck
        hlib.dontBenchmark
      ];

    in lib.pipe haskellPkg confFns;

  buildFaster = haskellPkg:
    let
      confFns = [
        hlib.dontHyperlinkSource
        hlib.dontCoverage
        hlib.dontHaddock
        hlib.disableExecutableProfiling
        hlib.disableLibraryProfiling
        hlib.dontBenchmark
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
        entry = let opts = [ "--check" ];
        in "${devShellEnv}/bin/nixfmt ${toolOpts opts}";
      };

      ormolu = {
        enable = true;
        package = devShellEnv;
        entry = let opts = [ "--mode" "check" "--check-idempotence" ];
        in "${devShellEnv}/bin/ormolu ${toolOpts opts}";
      };

      shfmt = {
        enable = true;
        package = devShellEnv;
        entry = let
          opts =
            [ "--indent" "0" "--binary-next-line" "--func-next-line" "--diff" ];
        in "${devShellEnv}/bin/shfmt ${toolOpts opts}";
      };

      shellcheck = {
        enable = true;
        package = devShellEnv;
      };

      parinfer-rust = {
        name = "parinfer-rust";
        enable = true;
        always_run = true;
        entry = "./parinfer-rust.sh";
        files = "\\.(clj|cljs|cljc|edn)$";
      };

      unit-test = {
        name = "unit-test.sh";
        enable = true;
        always_run = true;
        entry = "./unit-test.sh";
        pass_filenames = false;
        stages = [ "pre-push" ]; # (ref:pre-push)
      };

      vale = {
        enable = true;
        package = devShellEnv;
      };

      sort-file-contents = {
        enable = true;
        files = "\\.vale/styles/config/vocabularies/Lilac/accept\\.txt$";
        settings = { ignore-case = true; };
      };
    };
  };

  toolOpts = opts: builtins.concatStringsSep " " opts;
in {
  devShell = pkgs.mkShell {
    buildInputs = [ devShellEnv ]; # (ref:devShellEnv)
    shellHook = ''
      # Create a local cabal config that points to Nix store. This prevents
      # cabal from downloading and instead uses the GHC package DB.
      export CABAL_CONFIG="$PWD/cabal.config.nix"
      echo "package-db: $(ghc --print-libdir)/package.conf.d" > cabal.config.nix
      echo "with-compiler: $(which ghc)" >> cabal.config.nix

      export LD_LIBRARY_PATH=${devShellEnv}/lib:$LD_LIBRARY_PATH
      # Used when building with cabal.
      export LILAC_PROJECT_ROOT=$PWD
      # Set up Git pre-commit hooks.
      ${pre-commit-check.shellHook}
      logo
    '';
  }; # (ref:devShell drv)
  lilac = ourHaskell.lilac; # (ref:lilac drv)
}
