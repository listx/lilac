build-quick: lilac.org lilac.cabal
	cabal build

build-release: lilac.org lilac.cabal
	nix-build ./nix/release.nix --argstr compiler 9.10.1
lint: lilac.org
	./check-tangled.sh
	./check-newlines.sh
	treefmt --fail-on-change
test: lilac.org
	cabal test --ghc-options="-fforce-recomp"
update-nix:
	niv update nixpkgs --branch nixos-24.11
