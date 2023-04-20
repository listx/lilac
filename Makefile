PROJ_ROOT := $(shell git rev-parse --show-toplevel)
PROCS := $(shell nproc)
define run_emacs
	emacs $(2) --quick --batch --kill --load $(PROJ_ROOT)/lilac.el --eval="$(1)"
endef

all: test weave
.PHONY: all

# Currently we don't have any optimizations for tangling, but we still set LILAC_LP_QUICK=1 anyway to align with what we do for weave-quick.
$(README_org_output) tangle &: README.org
	@echo tangling in parallel
	LILAC_LP_QUICK=1 make -C $(PROJ_ROOT) -j$(PROCS) $(README_org_output)
	touch tangle

README_org_output = citations.bib lilac.el lilac.theme .gitattributes .gitignore Makefile misc.js shell.nix style.css syntax-highlighting.css

$(README_org_output) &: README.org
	# Generate the toplevel Makefile (this file) and image/Makefile (overwriting
	# them if necessary). In a way this bootstraps the whole
	# literate-programming pipeline. Note that these files are different than
	# the ones used to compile the tangled source code.
	$(call run_emacs,(org-babel-tangle),README.org)

define tangle_tests

$(1) $(2) &: $(3)
	@echo tangling $(3)
	$(call run_emacs,(org-babel-tangle),$(3))

endef

weave: build-html

build-html: README.html
.PHONY: build-html

README.html: README.org citations.bib
	$(call run_emacs,(batch-org-gen-css-and-exit \"README.org\"),)
	$(call run_emacs,(lilac-publish),README.org)
	sed -i 's/.csl-left-margin{float: left; padding-right: 0em/.csl-left-margin{float: left; padding-right: 1em/' README.html
	sed -i 's/.csl-right-inline{margin: 0 0 0 1em/.csl-right-inline{margin: 0 0 0 2em/' README.html

test: tangle
	# TODO: Add unit tests
.PHONY: test

# Enter development environment.
shell:
	nix-shell --pure
