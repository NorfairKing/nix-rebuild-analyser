{ mkDerivation, aeson, aeson-pretty, base, bytestring, lib
, monad-logger, nix-rebuild-analyser, path, path-io, sydtest
, sydtest-discover, typed-process
}:
mkDerivation {
  pname = "nix-rebuild-analyser-test";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = false;
  testHaskellDepends = [
    aeson aeson-pretty base bytestring monad-logger
    nix-rebuild-analyser path path-io sydtest typed-process
  ];
  testToolDepends = [ sydtest-discover ];
  license = "GPL";
}
