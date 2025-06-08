test: lilac.org lilac-parse.org lilac-weave.org
	cabal test --test-show-details=direct --test-option=--format=progress
	touch test

lint: lilac.org lilac-parse.org lilac-weave.org
	pre-commit run --all-files
	touch lint

tangle: lilac.org lilac-parse.org lilac-weave.org tangle-fe
	git ls-files | grep '\.org$$' | xargs lilac write-goids
	cd test && find Lilac/TangleSpec -type f \
		| sort \
		| xargs lilac write-goids --out-file .orgids
	lilac tangle \
		lilac-parse.org \
		lilac-weave.org \
		lilac.org
	touch tangle
tangle-fe: fe.org
	lilac tangle fe.org
	touch tangle-fe

weave: lilac.org lilac-parse.org lilac-weave.org fe.org
	lilac weave --out-dir site --watch \
		lilac-parse.org \
		lilac-weave.org \
		lilac.org
	touch weave

serve:
	python3 -m http.server 8080 -d site

build: lilac.org lilac-parse.org lilac-weave.org lilac.cabal
	cabal build
	touch build

LILAC_GIT_VERSION := $(shell git describe --abbrev=10 --always --dirty)
build-release: lilac.org lilac-parse.org lilac-weave.org lilac.cabal
	nix-build ./nix/release.nix \
	  --argstr lilacGitVersion $(LILAC_GIT_VERSION)
	touch build-release

update-nix:
	niv init
	niv update nixpkgs --branch nixos-25.05
