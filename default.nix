{ mkDerivation, aeson, base, bytestring, containers, data-default
, dependent-sum, ghcjs-dom, jsaddle, lens, scientific, stdenv, text
, time, unordered-containers, vector
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
  hydraPlatforms = stdenv.lib.platforms.none;
}
