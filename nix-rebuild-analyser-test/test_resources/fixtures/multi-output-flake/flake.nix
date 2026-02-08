{
  description = "Multi-output test flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      libSrc = ./lib;
      appSrc = ./app;
    in
    {
      packages.${system} = {
        default = self.packages.${system}.myapp;

        mylib = pkgs.stdenv.mkDerivation {
          name = "mylib";
          src = libSrc;
          buildPhase = "cat $src/Lib.hs > $out";
          installPhase = "true";
        };

        myapp = pkgs.stdenv.mkDerivation {
          name = "myapp";
          src = appSrc;
          buildPhase = "cat $src/Main.hs > $out";
          installPhase = "true";
        };
      };

      checks.${system}.test = pkgs.stdenv.mkDerivation {
        name = "test-check";
        src = appSrc;
        buildPhase = "echo 'test passed' > $out";
        installPhase = "true";
      };
    };
}
