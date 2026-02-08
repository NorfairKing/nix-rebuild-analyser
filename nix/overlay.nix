final: prev:
with final.haskell.lib;
{
  nix-rebuild-analyser =
    justStaticExecutables final.haskellPackages.nix-rebuild-analyser;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: super: {
      nix-rebuild-analyser = self.opt-env-conf.installManpagesAndCompletions [ "nix-rebuild-analyser" ]
        (buildStrictly (self.callPackage ../nix-rebuild-analyser { }));

    });
  });
}
