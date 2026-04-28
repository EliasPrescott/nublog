{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  # haskell.compiler.ghc967
  outputs = { self, nixpkgs }: let
    systems = [
      "aarch64-darwin"
      "aarch64-linux"
      "x86_64-darwin"
      "x86_64-linux"
    ];
    lib = nixpkgs.lib;
    eachSystem = lib.genAttrs systems;
    ghcVersion = "ghc9123";
  in {
    packages = eachSystem (sys: let
      pkgs = nixpkgs.legacyPackages.${sys};
      ghc = pkgs.haskell.compiler.${ghcVersion};
    in rec {
      default = pkgs.stdenv.mkDerivation {
        name = "nublog";
        src = ./.;
        buildPhase = ''
          ${ghc}/bin/runghc main.hs
          cp -r _build/ $out/
        '';
      };

      dev-server = pkgs.writeShellScriptBin "dev-server" ''
        ${pkgs.static-server}/bin/static-server ${default}
      '';
    });

    apps = eachSystem (sys: {
      default = {
        type = "app";
        program = "${self.packages.${sys}.dev-server}/bin/dev-server";
      };
    });

    devShells = eachSystem (sys: let
      pkgs = nixpkgs.legacyPackages.${sys};
      ghc = pkgs.haskell.compiler.${ghcVersion};
    in {
      default = {
        packages = [
          ghc
        ];
      };
    });
  };
}
