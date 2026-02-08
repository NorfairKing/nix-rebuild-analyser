{
  description = "Test flake with same-named files in different directories";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      # Sources for different packages
      pkgASrc = ./pkg-a;
      pkgBSrc = ./pkg-b;
    in
    {
      packages.${system} = {
        # Depends only on pkg-a/Util.hs
        pkg-a-only = pkgs.stdenv.mkDerivation {
          name = "pkg-a-only";
          src = pkgASrc;
          buildPhase = "cat $src/Util.hs > $out";
          installPhase = "true";
        };

        # Depends only on pkg-b/Util.hs
        pkg-b-only = pkgs.stdenv.mkDerivation {
          name = "pkg-b-only";
          src = pkgBSrc;
          buildPhase = "cat $src/Util.hs > $out";
          installPhase = "true";
        };

        # Depends on both pkg-a/Util.hs and pkg-b/Util.hs
        both = pkgs.stdenv.mkDerivation {
          name = "both";
          srcs = [ pkgASrc pkgBSrc ];
          unpackPhase = ''
            for src in $srcs; do
              cp -r $src/* .
            done
          '';
          buildPhase = "cat Util.hs > $out";
          installPhase = "true";
        };
      };
    };
}
