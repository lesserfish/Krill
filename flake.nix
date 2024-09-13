{
  description = "Development Environment for Krill";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }: {
    devShells = {
      x86_64-linux.default = let
        pkgs = import nixpkgs {
          system = "x86_64-linux";
          config.allowUnfree = true;
        };
      in pkgs.mkShell {
        packages = with pkgs; [
          bashInteractive
          ghc
          cabal-install
          haskell-language-server
          SDL2
        ];
        nativeBuildInputs = with pkgs; [
          pkg-config
        ];
      };

      aarch64-darwin.default = let
        pkgs = import nixpkgs {
          system = "aarch64-darwin";
          config.allowUnfree = true;
        };
      in pkgs.mkShell {
        packages = with pkgs; [
          bashInteractive
          ghc
          cabal-install
          haskell-language-server
          SDL2
        ];
        nativeBuildInputs = with pkgs; [
          pkg-config
        ];
      };
    };
  };
}

