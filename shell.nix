{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers
      , data-default, dependent-sum, ghcjs-dom, jsaddle, lens, lib
      , scientific, text, time, unordered-containers, vector
      }:
      mkDerivation {
        pname = "echarts-jsdom";
        version = "0.1";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring containers data-default dependent-sum
          ghcjs-dom jsaddle lens scientific text time unordered-containers
          vector
        ];
        license = "unknown";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
