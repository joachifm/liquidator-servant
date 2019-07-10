with (import <nixpkgs>{ config = {}; });

let
  hsDevEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
    async
    text

    aeson
    cassava
    lucid

    servant
    servant-lucid
    servant-server
    servant-swagger

    wai
    warp
  ]);
in

mkShell {
  name = "dev-shell";
  buildInputs = [ hsDevEnv ];
}
