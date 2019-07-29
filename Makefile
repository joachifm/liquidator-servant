.ONESHELL:
.SUFFIXES:
.DELETE_ON_ERROR:

all: cabal2nix configure

.PHONY: cabal2nix
cabal2nix: liquidator-redux-package.nix

.PHONY: configure
configure: cabal.project.local

.PHONY: build-package
build-package: cabal2nix
	nix-build release.nix -A liquidator-redux

liquidator-redux/liquidator-redux.cabal: liquidator-redux/package.yml
	( cd liquidator-redux && hpack package.yml )

liquidator-redux-package.nix: liquidator-redux/liquidator-redux.cabal
	cabal2nix liquidator-redux > liquidator-redux-package.nix.tmp
	mv liquidator-redux-package.nix.tmp liquidator-redux-package.nix

cabal.project.local: cabal.project
	./cabal new-configure \
		--disable-optimization \
		--enable-deterministic \
		--enable-tests \
		--enable-benchmarks \
		--enable-executable-dynamic \
		--disable-library-vanilla \
		--ghc-options="-dynamic"
