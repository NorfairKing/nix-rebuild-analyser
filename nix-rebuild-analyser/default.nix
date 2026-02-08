{ mkDerivation, aeson, base, bytestring, containers, lib
, monad-logger, opt-env-conf, path, path-io, safe-coloured-text
, safe-coloured-text-terminfo, text, typed-process, unliftio
}:
mkDerivation {
  pname = "nix-rebuild-analyser";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers monad-logger opt-env-conf path
    path-io safe-coloured-text safe-coloured-text-terminfo text
    typed-process unliftio
  ];
  executableHaskellDepends = [ base ];
  license = "GPL";
  mainProgram = "nix-rebuild-analyser";
}
