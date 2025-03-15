test: lilac.org
	cabal test --ghc-options="-fforce-recomp"
lint: lilac.org
	./check-tangled.sh
	pre-commit run --all-files
build: lilac.org lilac.cabal
	cabal build

LILAC_GIT_VERSION := $(shell git describe --abbrev=10 --always --dirty)
build-release: lilac.org lilac.cabal
	nix-build ./nix/release.nix \
	  --argstr lilacGitVersion $(LILAC_GIT_VERSION)
update-nix:
	niv init
	niv update nixpkgs --branch nixos-24.11
