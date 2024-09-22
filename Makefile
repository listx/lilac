build-quick: lilac.org lilac.cabal
	cabal build
build-release: lilac.org lilac.cabal
	nix-build ./nix/release.nix --argstr compiler 9.4.8
lint: lilac.org
	./check-tangled.sh
	./check-newlines.sh
	treefmt --fail-on-change
