{
  description = "hfs";

  inputs = {
    nixpkgs.url = "nixpkgs";
    typed-systems = {
      url = "github:YellowOnion/nix-typed-systems";
      flake = false;
    };
  };

  outputs = { self, typed-systems, nixpkgs }:
    let
      pkgName = "hfs";
      inherit (import typed-systems) id genAttrsMapBy systems';
      systems = [ systems'.x86_64-linux systems'.aarch64-linux ];

      eachSystem = genAttrsMapBy id (system:
        let pkgs = import nixpkgs {
              inherit system;
            };
            hsLib = pkgs.haskell.lib.compose;
        in {
          inherit system pkgs;
          haskellPkgs = pkgs.haskellPackages.override {
            overrides = _: super: {
          };
          };}) systems;
    in {
      packages = eachSystem ({ pkgs, haskellPkgs, ... }:
        let
          pkg = haskellPkgs.callCabal2nix pkgName self {};
        in {
          ${pkgName} = pkg;
          default = pkg;
        });

      devShells = eachSystem ({ pkgs, haskellPkgs, system }: {
        default = haskellPkgs.shellFor {
          withHoogle = true;
          packages = p: [ self.packages.${system}.default ];
          buildInputs = builtins.attrValues {
            inherit (pkgs) cabal-install cabal2nix;
            inherit (haskellPkgs) ghc
              haskell-language-server
              lambdabot
              hlint
              eventlog2html;
          };
        };});

      formatter = eachSystem ({ pkgs, ... }: pkgs.nixfmt);
    };
}
