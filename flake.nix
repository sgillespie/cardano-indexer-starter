{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt.url = "github:numtide/treefmt-nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, treefmt }@attrs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc966";

          shell.tools = {
            cabal = { };
            cabal-gild = { };
            fourmolu = { };
            hlint = { };
            haskell-language-server = { };
          };

          shell.buildInputs = with pkgs; [
            nixpkgs-fmt
          ];
        };

        flake = project.flake { };

        mkFormatter = { pkgs, lib, ... }: {
          projectRootFile = "flake.nix";

          programs = {
            fourmolu.enable = true;
            hlint.enable = true;
            mdformat.enable = true;
            nixpkgs-fmt.enable = true;
          };

          settings = {
            global.excludes = [
              "LICENSE"
            ];

            formatter.cabal-gild = {
              command = "${pkgs.bash}/bin/bash";
              options = [
                "-euc"
                ''
                  for file in "$@"; do
                    ${lib.getExe pkgs.haskellPackages.cabal-gild} --io="$file" --mode=format
                  done
                ''
                "--"
              ];

              includes = [ "*.cabal" "cabal.project" ];
            };
          };
        };

        formatter = (treefmt.lib.evalModule pkgs mkFormatter).config.build;

      in
      flake // {
        formatter = formatter.wrapper;

        packages.default = flake.packages."cardano-indexer-starter:exe:cardano-indexer-starter";
        checks.formatting = formatter.check self;
      });

  nixConfig = {
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    substituters = [
      "https://cache.nixos.org/"
      "https://cache.iog.io"
    ];

    allow-import-from-derivation = "true";
    experimental-features = [ "nix-command flakes" ];
  };
}
