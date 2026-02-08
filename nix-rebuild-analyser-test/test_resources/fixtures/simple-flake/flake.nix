{
  description = "Simple test flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.stdenv.mkDerivation {
        name = "simple-package";
        src = ./src;
        buildPhase = "cat $src/Main.hs > $out";
        installPhase = "true";
      };
    };
}
