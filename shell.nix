with (import <nixpkgs> { config = {}; });

let

  devTools = [
    httpie
    httperf
  ];

  liquidator-redux = haskellPackages.callPackage ./liquidator-redux-package.nix {};

  hsDevEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
    cabal-install
    cabal2nix

    ghcid
    hlint

    doctest
    hspec-discover
  ]);

in

mkShell {
  name = "dev-shell";
  buildInputs = devTools ++ [ hsDevEnv ];
  inherit hsDevEnv;

  passthru = {
    inherit liquidator-redux;
  };
}
