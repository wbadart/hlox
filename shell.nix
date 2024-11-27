let
  sources = import ./npins;
  nixpkgs = import sources.nixpkgs {
    config = {};
    overlays = [];
  };
in
nixpkgs.mkShell {
  packages = with nixpkgs; [
    cabal-install
    ghc
    haskell-language-server
  ];
}
