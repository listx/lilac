define INSTALL_HELP
To install Lilac, run either:
	- 'make install' (Cabal)
	- 'make install-release' (Nix)
depending on how you want to install it.
endef
export INSTALL_HELP

all: build test weave
	@echo "$$INSTALL_HELP" | sed 's/^\t/  /'
.PHONY: all

org_files := $(shell find . -maxdepth 1 -type f -name "*.org" && find ./design -type f -name "*.org")
backend_org_files = $(filter-out fe.org,$(org_files))

test: tangle
	cabal test --test-show-details=direct --test-option=--format=progress
	touch test

lint: $(archive_file) tangle
	lilac lint $(archive_file)
	pre-commit run --all-files
	touch lint #

obj_files = $(org_files:%.org=%.lobj)

$(obj_files) &: $(org_files)
	lilac compile $?
	touch $(obj_files) #

archive_file := lilac.larc

$(archive_file): $(obj_files)
	lilac archive $^ --out-file $@

tangle: $(archive_file)
	lilac tangle $(archive_file) --out-dir .
	touch tangle

image_sources = $(wildcard image/*.pikchr)
image_svgs = $(image_sources:.pikchr=.svg)

%.svg: %.pikchr
	@echo "Compiling Pikchr diagram: $<"
	pikchr --svg-only $< > $@

#
vendored := \
	vendor/source-fonts \
	vendor/mathjax \
	vendor/mathjax-newcm-font

site.tar.gz: $(archive_file) weave $(vendored)
	rm -f site.tar.gz
	tar czvf site.tar.gz -C site .

vendor/source-fonts:
	./vendor-source-fonts.sh

vendor/mathjax vendor/mathjax-newcm-font &:
	./vendor-mathjax.sh

PAGES_BRANCH := gh-pages
PAGES_DOMAIN := lilac.funloop.org
PAGES_REMOTE ?= $(shell git remote get-url $$(git config branch.main.remote) || git remote get-url origin)
PAGES_GIT_NAME  ?= $(or $(shell git config user.name),pages-publisher)
PAGES_GIT_EMAIL ?= $(or $(shell git config user.email),pages-publisher@localhost)

publish: $(archive_file) weave $(vendored)
	rm -rf site/vendor
	cp -r vendor site
	touch site/.nojekyll
	echo $(PAGES_DOMAIN) > site/CNAME
	cd site && \
		git add -A && \
		{ git diff --cached --quiet || \
			git -c user.name="$(PAGES_GIT_NAME)" -c user.email="$(PAGES_GIT_EMAIL)" \
			commit -q -m "update"; } && \
		git push -f "$(PAGES_REMOTE)" $(PAGES_BRANCH)
	touch publish

weave: $(image_svgs) $(archive_file) tangle $(vendored)
	rm -rf site/image site/vendor
	lilac weave $(archive_file) --out-dir site --write-css #
	cp -r image vendor site
	touch weave

dev-cljs: tangle
	clojure -M -m figwheel.main --build lilac --repl
.PHONY: dev-cljs

prod-cljs: tangle
	clojure -M -m figwheel.main --build-once lilac-prod

	cp -f site/js/lilac.js lilac.js.injectme
.PHONY: prod-cljs

build: tangle lilac.cabal
	cabal build
	touch build

install: tangle lilac.cabal
	cabal install --overwrite-policy=always
	touch install

LILAC_CABAL_VERSION = $(shell sed -n 's/^[Vv]ersion:[[:space:]]*//p' lilac.cabal | head -n1)
LILAC_GIT_COMMIT    = $(shell git rev-parse --short=10 HEAD)
LILAC_GIT_DIRTY     = $(shell git diff-index --quiet HEAD -- || echo -dirty)
LILAC_GIT_VERSION   = v$(LILAC_CABAL_VERSION)-g$(LILAC_GIT_COMMIT)$(LILAC_GIT_DIRTY)
build-release: tangle lilac.cabal
	nix-build ./nix/release.nix \
		--argstr lilacGitVersion $(LILAC_GIT_VERSION)
	touch build-release

install-release: tangle result build-release
	nix-env -i ./result
	touch install-release

push-release: build-release
	cachix push funloop ./result

update-nix: tangle
	niv init
	niv update nixpkgs --branch nixos-26.05

clean:
	rm -rf \
		$(archive_file) \
		$(obj_files)
.PHONY: clean

gitconfig:
	git config diff.orderfile .git-orderfile
.PHONY: gitconfig
