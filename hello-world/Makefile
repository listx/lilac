org_files = \
	doc/index.org #

obj_files = $(org_files:%.org=%.lobj) #

archive_file = doc/hello-world.larc #

all: tangle weave lint
.PHONY: all

$(obj_files) &: $(org_files) #
	lilac compile $?
	touch $(obj_files)

$(archive_file): $(obj_files) #
	lilac archive $^ --out-file $@

tangle: $(archive_file) #
	lilac tangle $< --out-dir . #
	touch tangle

weave: $(archive_file) #
	lilac weave $< --out-dir site --write-css --write-js
	touch weave

lint: $(archive_file) #
	lilac lint $<
	touch lint

clean:
	rm -rf \
		$(archive_file) \
		$(obj_files)
.PHONY: clean

gitconfig:
	git config diff.orderfile .git-orderfile
.PHONY: gitconfig
