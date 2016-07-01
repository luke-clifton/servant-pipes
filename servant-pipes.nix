{ mkDerivation, base, bytestring, cassava, http-client, http-media
, http-types, mtl, pipes, pipes-bytestring, pipes-csv, pipes-safe
, servant, servant-cassava, servant-client, servant-server, stdenv
, vector, wai, warp
}:
mkDerivation {
  pname = "servant-pipes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava http-client http-media http-types mtl pipes
    pipes-bytestring pipes-csv pipes-safe servant servant-cassava
    servant-client servant-server wai
  ];
  executableHaskellDepends = [
    base cassava pipes servant servant-server vector wai warp
  ];
  description = "Streaming requests in Servant using Pipes";
  license = stdenv.lib.licenses.bsd3;
}
