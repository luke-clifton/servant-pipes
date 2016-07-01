{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, cassava, criterion
      , http-client, http-media, http-types, mtl, pipes, pipes-bytestring
      , pipes-csv, pipes-safe, servant, servant-cassava, servant-client
      , servant-server, stdenv, wai
      }:
      mkDerivation {
        pname = "servant-pipes";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring cassava http-client http-media http-types mtl pipes
          pipes-bytestring pipes-csv pipes-safe servant servant-cassava
          servant-client servant-server wai
        ];
        testHaskellDepends = [ async base criterion pipes ];
        description = "Streaming requests in Servant using Pipes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
