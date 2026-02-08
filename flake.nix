{
  description = "nix-rebuild-analyser";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , autodocodec
    , safe-coloured-text
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          (import (validity + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
        ];
      };
    in
    {
      overlays.default = import ./nix/overlay.nix;

      packages.${system} = {
        default = pkgs.nixRebuildAnalyserRelease;
        nix-rebuild-analyser = pkgs.nixRebuildAnalyserReleasePackages.nix-rebuild-analyser;
      };

      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [
              ".*/default.nix"
              ".*/options.nix"
            ];
            cabal2nix.enable = true;
          };
        };
      };

      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "nix-rebuild-analyser-shell";
        packages = p: builtins.attrValues p.nixRebuildAnalyserPackages;
        withHoogle = true;
        buildInputs = with pkgs; [
          cabal-install
          git
          nix
          zlib
        ] ++ self.checks.${system}.pre-commit.enabledPackages;

        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
