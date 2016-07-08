{ mkDerivation, base, bytestring, cassava, http-client, http-media
, http-types, lens, mtl, pipes, pipes-bytestring, pipes-csv
, pipes-safe, servant, servant-cassava, servant-client
, servant-docs, servant-server, stdenv, text, unordered-containers
, vector, wai, warp
}:
mkDerivation {
  pname = "servant-pipes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava http-client http-media http-types lens mtl
    pipes pipes-bytestring pipes-csv pipes-safe servant servant-cassava
    servant-client servant-docs servant-server text
    unordered-containers wai
  ];
  executableHaskellDepends = [
    base cassava pipes servant servant-cassava servant-docs
    servant-server vector wai warp
  ];
  description = "Streaming requests in Servant using Pipes";
  license = stdenv.lib.licenses.bsd3;
}
