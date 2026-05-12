gitconfig:
	git config diff.orderfile .git-orderfile
.PHONY: gitconfig

org_files = \
	index.org \
	cli.org \
	build.org \
	compile.org \
	parse.org \
	tangle.org \
	types.org \
	weave.org \
	serve.org \
	fe.org \
	user-manual.org \
	plan.org \
	release-notes.org \
	hello-world/index.org \
	hello-world/doc/index.org
backend_org_files = $(filter-out fe.org,$(org_files))

test: $(org_files)
	cabal test --test-show-details=direct --test-option=--format=progress
	touch test

lint: lilac.larc
	lilac lint lilac.larc
	pre-commit run --all-files
	touch lint #

obj_files = $(org_files:%.org=%.lobj)

$(obj_files) &: $(org_files)
	lilac compile $?
	touch $(obj_files) #

lilac.larc: $(obj_files)
	lilac archive $^ --out-file $@

tangle: lilac.larc
	lilac tangle lilac.larc --out-dir .
	touch tangle

weave: lilac.larc
	lilac weave $< --out-dir . --write-css #
	touch weave

dev-cljs:
	clojure -M -m figwheel.main --build lilac --repl
.PHONY: cljs

prod-cljs:
	clojure -M -m figwheel.main --build-once lilac-prod

	cp -f js/lilac.js js/lilac.js.injectme
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
		lilac.larc \
		$(obj_files)
.PHONY: clean
