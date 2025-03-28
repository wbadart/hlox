let
  sources = import ./npins;
  nixpkgs = import sources.nixpkgs {
    config = {};
    overlays = [];
  };
in
nixpkgs.mkShell {
  packages = with nixpkgs; [
    ghcid
    (ghc.withPackages (haskell-pkgs: with haskell-pkgs; [
      bluefin
      cabal-install
      microlens

      haskell-language-server
    ]))
  ];
}
