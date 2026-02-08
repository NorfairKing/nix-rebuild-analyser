final: prev:
with final.lib;
with final.haskell.lib;
{
  nixRebuildAnalyserRelease =
    final.symlinkJoin {
      name = "nix-rebuild-analyser-release";
      paths = builtins.attrValues final.nixRebuildAnalyserReleasePackages;
    };

  nixRebuildAnalyserReleasePackages =
    mapAttrs
      (_: pkg: justStaticExecutables pkg)
      final.haskellPackages.nixRebuildAnalyserPackages;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: super:
      let
        nixRebuildAnalyserPkg = name:
          buildFromSdist (overrideCabal
            (self.callPackage (../${name}) { })
            (old: {
              configureFlags = (old.configureFlags or [ ]) ++ [
                "--ghc-options=-O2"
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Wunused-packages"
                "--ghc-options=-Werror"
              ];
              doBenchmark = true;
              doHaddock = false;
              doCoverage = false;
              doHoogle = false;
              doCheck = false;
              hyperlinkSource = false;
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;
            }));

        nix-rebuild-analyser = self.opt-env-conf.installManpagesAndCompletions [ "nix-rebuild-analyser" ] (nixRebuildAnalyserPkg "nix-rebuild-analyser");

        nixRebuildAnalyserPackages = {
          inherit nix-rebuild-analyser;
        };
      in
      {
        inherit nixRebuildAnalyserPackages;
      } // nixRebuildAnalyserPackages
    );
  });
}
