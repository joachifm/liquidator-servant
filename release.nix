{ lib ? import (<nixpkgs> + "/lib")
, pkgs ? import <nixpkgs> { config = {}; }
, selfSrc ? { outPath = pkgs.cleanSource ./.; }
}:

with lib;
with pkgs;

let

  inherit selfSrc;

  devTools = [
    cachix
    nix

    httperf
    httpie
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

      liquidator-redux = self.callCabal2nix "liquidator-redux" ./liquidator-redux {};

    };
  };

  hsDevEnv = haskellPackagesLocal.ghcWithPackages (hsPkgs: with hsPkgs; [
    cabal-install
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
