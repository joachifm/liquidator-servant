{ lib ? import (<nixpkgs> + "/lib")
, pkgs ? import <nixpkgs> { config = {}; }
, selfSrc ? { outPath = pkgs.cleanSource ./.; }
}:

with lib;
with pkgs;

let

  inherit selfSrc;

  devTools = [
    httpie
    httperf
  ];

  liquidator-redux = haskellPackages.callPackage ./liquidator-redux-package.nix {};

  hsDevEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
    cabal-install
    cabal2nix
    hpack

    ghcid
    hlint

    doctest
    hspec-discover
  ]);

in

rec {

  devShell = mkShell {
    name = "dev-shell";
    buildInputs = devTools ++ [ hsDevEnv ];
    inherit hsDevEnv;
  };

  inherit hsDevEnv;

  inherit liquidator-redux;

}
