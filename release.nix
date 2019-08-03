{ lib ? import (<nixpkgs> + "/lib")
, pkgs ? import <nixpkgs> { config = {}; }
, selfSrc ? { outPath = pkgs.cleanSource ./.; }
}:

with lib;
with pkgs;

let

  inherit selfSrc;

  # Tools to be added to our development shell environment
  devTools = [
    # Shell tools
    coreutils
    diffutils
    findutils
    gawk
    gnugrep
    gnumake
    gnused

    # Nix
    cachix
    nix

    # Testing HTTP
    curl
    httperf
    httpie
  ];

  # A custom Haskell package set containing our local packages
  haskellPackagesLocal = haskellPackages.override {
    overrides = self: super: {

      liquidator-redux = self.callCabal2nix "liquidator-redux" ./liquidator-redux {};

    };
  };

  # A custom GHC distribution
  hsDevEnv = haskellPackagesLocal.ghcWithPackages (hsPkgs: with hsPkgs; [
    cabal-install
    hpack

    ghcid
    # hasktags
    # hindent
    # hlint

    doctest
    hspec-discover
  ]);

in

rec {

  inherit selfSrc;

  devShell = mkShell {
    name = "dev-shell";
    buildInputs = devTools ++ [ hsDevEnv ];
    inherit hsDevEnv;
    shellHook = ''
      # Set NIX_GHC et al.
      eval $(LC_ALL=C egrep '^export' ${hsDevEnv}/bin/ghc)
    '';
  };

  inherit hsDevEnv;

  inherit haskellPackagesLocal;

}
