org_files = \
	base/lilac.org \
	base/lilac-compile.org \
	base/lilac-parse.org \
	base/lilac-tangle.org \
	base/lilac-types.org \
	base/lilac-weave.org \
	base/fe.org
backend_org_files = $(filter-out base/fe.org,$(org_files))

test: $(org_files)
	cabal test --test-show-details=direct --test-option=--format=progress
	touch test

lint: $(org_files)
	pre-commit run --all-files
	touch lint

obj_files = $(org_files:%.org=%.lobj)

$(obj_files) &: $(org_files)
	lilac compile $?
	touch $(obj_files)

base/lilac.larc: $(obj_files)
	lilac archive $^ --out-file $@

tangle: base/lilac.larc
	lilac tangle base/lilac.larc --out-dir .
	touch tangle

weave: base/lilac.larc
	lilac weave --watch $<
	touch weave

serve:
	python3 -m http.server 8080 -d .

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

clean:
	rm -rf \
		base/lilac.larc \
		$(obj_files) \
		site/*
.PHONY: clean
