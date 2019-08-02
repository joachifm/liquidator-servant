.DELETE_ON_ERROR:
.ONESHELL:
.SUFFIXES:

liquidator-redux/liquidator-redux.cabal: liquidator-redux/package.yaml
	( cd liquidator-redux && hpack package.yaml )

cabal.project.local: cabal.project
	./cabal new-configure \
		--disable-optimization \
		--enable-deterministic \
		--enable-tests \
		--enable-benchmarks \
		--enable-executable-dynamic \
		--disable-library-vanilla \
		--ghc-options="-dynamic"
