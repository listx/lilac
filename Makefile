test: lilac.org
	cabal test --ghc-options="-fforce-recomp"
lint: lilac.org
	./check-tangled.sh
	./check-newlines.sh
	treefmt --fail-on-change
build: lilac.org lilac.cabal
	cabal build

LILAC_GIT_VERSION := $(shell git describe --abbrev=10 --always --dirty)
build-release: lilac.org lilac.cabal
	nix-build ./nix/release.nix \
	  --argstr lilacGitVersion $(LILAC_GIT_VERSION)
update-nix:
	niv init
	niv update nixpkgs --branch nixos-24.11
