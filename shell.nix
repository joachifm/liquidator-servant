with (import <nixpkgs>{ config = {}; });

let
  hsDevEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
    cabal-install
    ghcid

    async
    text

    aeson
    cassava
    lucid

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
