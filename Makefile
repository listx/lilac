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
	release-notes.org
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

weave: $(image_svgs) $(archive_file) tangle
	rm -rf site/image site/vendor
	cp -r image vendor site
	lilac weave $(archive_file) --out-dir site --write-css #
	touch weave

site.tar.gz: $(archive_file) weave vendor/mathjax vendor/mathjax-newcm-font vendor/source-fonts
	rm -f site.tar.gz
	tar czvf site.tar.gz -C site .

vendor/source-fonts:
	./vendor-source-fonts.sh

vendor/mathjax vendor/mathjax-newcm-font &:
	./vendor-mathjax.sh

SRHT_TOKEN != cat .srht-pages-token 2>/dev/null || echo ""

publish: weave site.tar.gz
	@if [ -z "$(SRHT_TOKEN)" ]; then \
		echo "Error: .srht-pages-token is empty or missing." >&2; \
		exit 1; \
	fi
	@echo "Uploading site to SourceHut Pages..."
	@curl --oauth2-bearer "$(SRHT_TOKEN)" \
		-Fcontent=@site.tar.gz \
		https://pages.sr.ht/publish/lilac.funloop.org
	touch publish

dev-cljs: tangle
	clojure -M -m figwheel.main --build lilac --repl
.PHONY: dev-cljs

prod-cljs: tangle
	clojure -M -m figwheel.main --build-once lilac-prod

	cp -f js/lilac.js js/lilac.js.injectme
.PHONY: prod-cljs

build: tangle lilac.cabal
	cabal build
	touch build

install: tangle lilac.cabal
	cabal install --overwrite-policy=always
	touch install

LILAC_GIT_VERSION := $(shell git describe --abbrev=10 --always --dirty)
build-release: tangle lilac.cabal
	nix-build ./nix/release.nix \
		--argstr lilacGitVersion $(LILAC_GIT_VERSION)
	touch build-release

install-release: tangle result build-release
	nix-env -i ./result
	touch install-release

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
