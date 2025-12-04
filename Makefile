org_files = \
	base/lilac.org \
	base/cli.org \
	base/build.org \
	base/compile.org \
	base/parse.org \
	base/tangle.org \
	base/types.org \
	base/weave.org \
	base/fe.org \
	base/artifacts/user-manual.org \
	base/artifacts/plan.org
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
	lilac weave $< --out-dir base --write-css #
	touch weave

dev-cljs:
	clojure -M -m figwheel.main --build lilac --repl
.PHONY: cljs

prod-cljs:
	clojure -M -m figwheel.main --build-once lilac-prod

	cp -f base/js/lilac.js base/js/lilac.js.injectme
.PHONY: prod-cljs

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
