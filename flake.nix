{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.bashInteractive
            pkgs.haskell.compiler.ghc910
            pkgs.haskell-language-server
            pkgs.cabal-install
            pkgs.pkg-config
          ];
          buildInputs = [
            pkgs.zlib
          ];
        };
      });
}