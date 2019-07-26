with (import <nixpkgs> { config = {}; });

let
  devTools = [
    httpie
    httperf
  ];

  hsDevEnv = haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
    cabal-install
    cabal2nix
    ghcid
    hlint
    hspec-discover
  ]);
in

mkShell {
  name = "dev-shell";
  buildInputs = devTools ++ [ hsDevEnv ];
  inherit hsDevEnv;
}
