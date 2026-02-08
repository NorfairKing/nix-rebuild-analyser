{
  description = "Test flake with same directory/file path but different store paths";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      # Two different sources with identical internal structure (lib/Util.hs)
      # These will have different store paths
      srcA = ./src-a;
      srcB = ./src-b;
    in
    {
      packages.${system} = {
        # Depends on src-a/lib/Util.hs (store path will be /nix/store/xxx-src-a/lib/Util.hs)
        uses-a = pkgs.stdenv.mkDerivation {
          name = "uses-a";
          src = srcA;
          buildPhase = "cat $src/lib/Util.hs > $out";
          installPhase = "true";
        };

        # Depends on src-b/lib/Util.hs (store path will be /nix/store/yyy-src-b/lib/Util.hs)
        uses-b = pkgs.stdenv.mkDerivation {
          name = "uses-b";
          src = srcB;
          buildPhase = "cat $src/lib/Util.hs > $out";
          installPhase = "true";
        };

        # Depends on both lib/Util.hs files from different store paths
        uses-both = pkgs.stdenv.mkDerivation {
          name = "uses-both";
          srcs = [ srcA srcB ];
          unpackPhase = ''
            mkdir -p a b
            cp -r ${srcA}/* a/
            cp -r ${srcB}/* b/
          '';
          buildPhase = "cat a/lib/Util.hs b/lib/Util.hs > $out";
          installPhase = "true";
        };
      };
    };
}
