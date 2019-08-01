{ mkDerivation, aeson, aeson-casing, base, bytestring, containers
, cookie, directory, ekg, fast-logger, hspec, hspec-discover
, http-api-data, http-types, jose, lens, lucid, mtl
, optparse-applicative, QuickCheck, servant, servant-auth
, servant-auth-server, servant-elm, servant-foreign, servant-js
, servant-lucid, servant-server, servant-swagger, stdenv, swagger2
, text, time, uuid, wai, wai-extra, warp, warp-tls
}:
mkDerivation {
  pname = "liquidator-redux";
  version = "0.1.0.0";
  src = ./liquidator-redux;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base bytestring containers cookie directory ekg
    fast-logger http-api-data http-types jose lens lucid mtl
    optparse-applicative servant servant-auth servant-auth-server
    servant-elm servant-foreign servant-js servant-lucid servant-server
    servant-swagger swagger2 text time uuid wai wai-extra warp warp-tls
  ];
  executableHaskellDepends = [
    aeson aeson-casing base bytestring containers cookie directory ekg
    fast-logger http-api-data http-types jose lens lucid mtl
    optparse-applicative servant servant-auth servant-auth-server
    servant-elm servant-foreign servant-js servant-lucid servant-server
    servant-swagger swagger2 text time uuid wai wai-extra warp warp-tls
  ];
  testHaskellDepends = [
    aeson aeson-casing base bytestring containers cookie directory ekg
    fast-logger hspec http-api-data http-types jose lens lucid mtl
    optparse-applicative QuickCheck servant servant-auth
    servant-auth-server servant-elm servant-foreign servant-js
    servant-lucid servant-server servant-swagger swagger2 text time
    uuid wai wai-extra warp warp-tls
  ];
  testToolDepends = [ hspec-discover ];
  license = stdenv.lib.licenses.agpl3Plus;
}
