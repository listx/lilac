test: lilac.org
	cabal test --test-show-details=direct --test-option=--format=progress
	touch test

lint: lilac.org
	pre-commit run --all-files
	touch lint

tangle: lilac.org tangle-fe
	git ls-files | grep '\.org$$' | xargs lilac write-goids
	lilac tangle lilac.org
	touch tangle
tangle-fe: fe.org
	lilac tangle fe.org
	touch tangle-fe

build: lilac.org lilac.cabal
	cabal build
	touch build

LILAC_GIT_VERSION := $(shell git describe --abbrev=10 --always --dirty)
build-release: lilac.org lilac.cabal
	nix-build ./nix/release.nix \
	  --argstr lilacGitVersion $(LILAC_GIT_VERSION)
	touch build-release

update-nix:
	niv init
	niv update nixpkgs --branch nixos-24.11
