with (import <nixpkgs>{ config = {}; });

let
  hsDevEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
    cabal2nix
    cabal-install
    ghcid
    hlint
    hspec-discover
  ]);
in

mkShell {
  name = "dev-shell";
  buildInputs = [ hsDevEnv ];
  inherit hsDevEnv;
}
