with (import <nixpkgs>{ config = {}; });

let
  hsDevEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
    cabal2nix
    cabal-install
    ghcid
    hlint

    async
    text

    aeson
    cassava
    lucid

    optparse-applicative

    servant
    servant-server

    servant-lucid
    servant-swagger

    wai
    warp
  ]);
in

mkShell {
  name = "dev-shell";
  buildInputs = [ hsDevEnv ];
}
