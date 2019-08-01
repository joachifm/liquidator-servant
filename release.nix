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

  shellTools = [
    coreutils
    findutils
    gawk
    gnugrep
    gnused
  ];

  haskellPackagesLocal = haskellPackages.override {
    overrides = self: super: {
      liquidator-redux = self.callPackage ./liquidator-redux-package.nix {};
    };
  };

  hsDevEnv = haskellPackagesLocal.ghcWithPackages (hsPkgs: with hsPkgs; [
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

  inherit selfSrc;

  devShell = mkShell {
    name = "dev-shell";
    buildInputs = shellTools ++ devTools ++ [ hsDevEnv ];
    inherit hsDevEnv;
    shellHook = ''
      # Set NIX_GHC et al.
      eval $(LC_ALL=C egrep '^export' ${hsDevEnv}/bin/ghc)
    '';
  };

  inherit hsDevEnv;

  inherit haskellPackagesLocal;

}
