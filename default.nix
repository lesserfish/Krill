
with import <nixpkgs> { };

mkShell {

  nativeBuildInputs = [
          bashInteractive
          ghc
          cabal-install
          haskell-language-server
          SDL2
          pkg-config
   ];
}

