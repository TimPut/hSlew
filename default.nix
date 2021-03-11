{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, parallel, tomland
      }:
      mkDerivation {
        pname = "hSlew";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base parallel tomland
        ];
        executableHaskellDepends = [
          base parallel tomland
        ];
        testHaskellDepends = [ base ];
        benchmarkHaskellDepends = [ base parallel tomland ];
        description = "Tool for designing differential planetary drives";
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
