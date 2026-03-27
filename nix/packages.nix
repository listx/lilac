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

    pkgs.editorconfig-checker

    pkgs.shellcheck

    pkgs.shfmt

    ourHaskell.nixfmt

    ourHaskell.ormolu

    pkgs.vale
  ];

  hlib = pkgs.haskell.lib;
  ghc = ourHaskell.ghc.withPackages
    (_: hlib.getHaskellBuildInputs ourHaskell.lilac);

  lib = pkgs.lib;
  ourHaskell = pkgs.haskell.packages.${ghcVersion}.extend
    (lib.composeManyExtensions haskellOverlays);

  ghcVersionWithoutDots = lib.replaceStrings [ "." ] [ "" ] "9.12.2";
  ghcVersion = "ghc" + ghcVersionWithoutDots;

  haskellOverlays = [
    jailbreaks
    injectExternalPackages
    injectLilac
    tweakLilacBuild
    buildDepsFast
  ];

  # (ref:nix:Haskell overlays-buildDepsFast)
  buildDepsFast = final: prev: {
    mkDerivation = args:
      prev.mkDerivation (args // {
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
        doBenchmark = false;
        doCheck = false;
        doHaddock = false;
      });
  };

  jailbreaks = final: prev: {
    skylighting-core = pkgs.haskell.lib.doJailbreak (prev.skylighting-core);

    unicode-data = pkgs.haskell.lib.dontCheck prev.unicode-data;
  };

  injectExternalPackages = final: prev: {
    xml-conduit = pkgs.haskell.lib.dontCheck (prev.callCabal2nix "xml-conduit"
      (let
        repo = fetchFromGitHub {
          owner = "snoyberg";
          repo = "xml";
          rev = "73ce67029c61decaa6525536377a15581325fd9e";
          sha256 = "sha256-Biu18KM+doPvVX1L+WYB/zgJmVUsSdcyDUfFaZza78E=";
          sparseCheckout = [ "xml-conduit" ];
        };
      in "${repo}/xml-conduit") { });

    citeproc = pkgs.haskell.lib.dontCheck (prev.callCabal2nix "citeproc" (let
      repo = fetchFromGitHub {
        owner = "jgm";
        repo = "citeproc";
        rev = "e9474a4fb31e2b2969375bd7829727eb242f240c";
        sha256 = "sha256-dArLwH9cvVa5Nv3kEDpWKQSUfA8YCd/4P/dTlCjV1co=";
      };
    in "${repo}") { });
  };

  injectLilac = final: prev: { # (ref:injectLilac)
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

  gnf =
    import (import ./sources.nix)."gitignore.nix" { inherit lib; }; # (ref:gnf)
  lilacSource = lib.cleanSourceWith {
    src = projectRoot;
    name = baseNameOf projectRoot + "-src"; # (ref:lilacSource name)
    filter = let # (ref:gnf-memoization)
      filterFunc = gnf.gitignoreFilterWith {
        basePath = projectRoot;
        extraRules = ''
          .*
          *.html
          *.nix
          *.sh
          *.toml
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
