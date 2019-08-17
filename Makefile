.DELETE_ON_ERROR:
.ONESHELL:
.SUFFIXES:

.PHONY: all
all: lint quickbuild test

.PHONY: repl
repl: configure
	./cabal new-repl liquidator-redux

.PHONY: build
build:
	nix build -f release.nix haskellPackagesLocal.liquidator-redux

.PHONY: quickbuild
quickbuild: configure
	./cabal new-build all

.PHONY: configure
configure: preconfigure cabal.project.local

.PHONY: preconfigure
preconfigure: liquidator-redux/liquidator-redux.cabal

.PHONY: test
test: preconfigure
	./cabal new-test all

.PHONY: lint
lint:
	nix-shell --run "hlint --no-exit-code --report liquidator-redux/src"


# Productions

liquidator-redux/liquidator-redux.cabal: liquidator-redux/package.yaml
	(cd liquidator-redux && hpack)

cabal.project.local: cabal.project
	./cabal new-configure \
		--disable-optimization \
		--enable-deterministic \
		--enable-tests \
		--enable-benchmarks \
		--enable-executable-dynamic \
		--disable-library-vanilla \
		--ghc-options="-dynamic"
