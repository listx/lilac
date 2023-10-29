all: test weave
.PHONY: all

test: tangle
	LILAC_ROOT=$(LILAC_ROOT) emacs --quick --batch --kill --load ert \
		--load lilac.el \
		--load lilac-tests.el \
		--funcall ert-run-tests-batch-and-exit
.PHONY: test

weave: lint README.html developer-guide.html
.PHONY: weave

README.html syntax-highlighting.css &: README.org
	$(call run_emacs,(lilac-gen-css-and-exit),$<)
	$(call run_emacs,(lilac-publish),$<)

developer-guide.html syntax-highlighting.css &: developer-guide.org
	$(call run_emacs,(lilac-gen-css-and-exit),$<)
	$(call run_emacs,(lilac-publish),$<)
# tangled_output are all files that are generated by tangling developer-guide.org.
tangled_output = \
	citations-developer-guide.bib \
	lilac.css \
	lilac.el \
	lilac-tests.el \
	lilac.js \
	lilac.theme \
	.gitattributes \
	.gitignore \
	Makefile \
	shell.nix

tangle $(tangled_output) &: developer-guide.org
	# Generate the toplevel Makefile (this file) and others as described in
	# tangled_output. In a way this bootstraps the whole literate-programming
	# pipeline.
	$(call run_emacs,(org-babel-tangle),developer-guide.org)
	touch tangle
define run_emacs
	LILAC_ROOT=$(LILAC_ROOT) emacs $(2) --quick --batch --kill \
		--load $(LILAC_ROOT)/lilac.el --eval="$(1)"
endef

LILAC_ROOT := $(shell git rev-parse --show-toplevel)
lint: spellcheck
.PHONY: lint

spellcheck: README.org developer-guide.org
	typos
.PHONY: spellcheck
nixpkgs_stable_channel := nixos-23.05
update-deps: package/nix/sources.json package/nix/sources.nix
	cd package && niv update nixpkgs --branch $(nixpkgs_stable_channel)
	cd package && niv update
	touch update-deps
