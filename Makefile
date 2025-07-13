org_files = \
	lilac.org \
	lilac-compile.org \
	lilac-parse.org \
	lilac-tangle.org \
	lilac-types.org \
	lilac-weave.org \
	fe.org
backend_org_files = $(filter-out fe.org,$(org_files))

test: $(org_files)
	cabal test --test-show-details=direct --test-option=--format=progress
	touch test

lint: $(org_files)
	pre-commit run --all-files
	touch lint

lilac.larc: $(org_files:%.org=%.lobj)
	lilac archive $^ --out-file lilac.larc
%.lobj: %.org
	lilac compile $<

tangle: $(backend_org_files) tangle-fe
	git ls-files | grep '\.org$$' | xargs lilac write-goids
	cd test && find Lilac/TangleSpec -type f \
		| sort \
		| xargs lilac write-goids --out-file .orgids
	lilac tangle $(backend_org_files)
	touch tangle
tangle-fe: fe.org
	lilac tangle fe.org
	touch tangle-fe

weave: $(org_files)
	lilac weave --out-dir site --watch $(org_files)
	touch weave

serve:
	python3 -m http.server 8080 -d site

build: $(org_files) lilac.cabal
	cabal build
	touch build

LILAC_GIT_VERSION := $(shell git describe --abbrev=10 --always --dirty)
build-release: $(org_files) lilac.cabal
	nix-build ./nix/release.nix \
	  --argstr lilacGitVersion $(LILAC_GIT_VERSION)
	touch build-release

update-nix:
	niv init
	niv update nixpkgs --branch nixos-25.05
