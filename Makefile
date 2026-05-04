gitconfig:
	git config diff.orderfile .git-orderfile
.PHONY: gitconfig

org_files = \
	index.org \
	doc/index.org \
	doc/cli.org \
	doc/build.org \
	doc/compile.org \
	doc/parse.org \
	doc/tangle.org \
	doc/types.org \
	doc/weave.org \
	doc/serve.org \
	doc/fe.org \
	doc/artifacts/index.org \
	doc/artifacts/user-manual.org \
	doc/artifacts/plan.org \
	doc/artifacts/release-notes.org \
	doc/artifacts/hello-world/index.org \
	doc/artifacts/hello-world/doc/index.org
backend_org_files = $(filter-out doc/fe.org,$(org_files))

test: $(org_files)
	cabal test --test-show-details=direct --test-option=--format=progress
	touch test

lint: $(org_files)
	pre-commit run --all-files
	touch lint #

obj_files = $(org_files:%.org=%.lobj)

$(obj_files) &: $(org_files)
	lilac compile $?
	touch $(obj_files) #

doc/lilac.larc: $(obj_files)
	lilac archive $^ --out-file $@

tangle: doc/lilac.larc
	lilac tangle doc/lilac.larc --out-dir .
	touch tangle

weave: doc/lilac.larc
	lilac weave $< --out-dir doc --write-css #
	touch weave

dev-cljs:
	clojure -M -m figwheel.main --build lilac --repl
.PHONY: cljs

prod-cljs:
	clojure -M -m figwheel.main --build-once lilac-prod

	cp -f doc/js/lilac.js doc/js/lilac.js.injectme
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
	niv update nixpkgs --branch nixos-25.11

clean:
	rm -rf \
		doc/lilac.larc \
		$(obj_files) \
		site/*
.PHONY: clean
