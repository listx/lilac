build-quick: lilac.org lilac.cabal
	cabal build
build-release: lilac.org lilac.cabal
	nix-build nix/release.nix
