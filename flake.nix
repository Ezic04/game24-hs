{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        
        haskellPackages = pkgs.haskell.packages.ghc910;

        project = haskellPackages.callCabal2nix "game24-hs" ./. {
          inherit (pkgs) zlib;
        };
      in
      {
        packages.default = project;

        devShells.default = haskellPackages.shellFor {
          packages = p: [ project ];
          nativeBuildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            pkgs.bashInteractive
            pkgs.pkg-config
          ];
          buildInputs = with pkgs; [
            zlib
          ];
        };
      });
}