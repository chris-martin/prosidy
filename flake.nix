{
  inputs = {
    "nixos-24.11".url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "prosidy";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-24.11" = import inputs."nixos-24.11" { inherit system; };
        };
        pkgs = nixpkgs."nixos-24.11";
        project = pkgs.haskellPackages.developPackage {
          root = ./.;
          name = packageName;
        };
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages = let

          inherit (pkgs.haskell.lib) dontCheck;

          makeTestConfiguration = let defaultPkgs = pkgs;
          in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
          let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
          in (pkgs.haskell.packages.${ghcVersion}.override (old: {
            overrides = combineOverrides old [
              (packageSourceOverrides { prosidy = ./.; })
              overrides
            ];

          })).prosidy;

        in rec {
          "${packageName}" = project;

          ghc-9-6 = makeTestConfiguration {
            pkgs = nixpkgs."nixos-24.11";
            ghcVersion = "ghc96";
          };
          ghc-9-8 = makeTestConfiguration {
            pkgs = nixpkgs."nixos-24.11";
            ghcVersion = "ghc98";
          };
          # ghc-9-4 = makeTestConfiguration {
          #   pkgs = nixpkgs."nixos-24.11";
          #   ghcVersion = "ghc94";
          #   overrides = new: old: {
          #     aeson = dontCheck (new.callPackage ./nix/aeson-2.1.2.nix { });
          #     base-compat =
          #       dontCheck (new.callPackage ./nix/base-compat-0.13.0.nix { });
          #     base-compat-batteries = dontCheck
          #       (new.callPackage ./nix/base-compat-batteries-0.13.0.nix { });
          #     foldable1-classes-compat = dontCheck
          #       (new.callPackage ./nix/foldable1-classes-compat-0.1.nix { });
          #     OneTuple =
          #       dontCheck (new.callPackage ./nix/OneTuple-0.4.1.nix { });
          #     quickcheck-instances = dontCheck
          #       (new.callPackage ./nix/quickcheck-instances-0.3.29.nix { });
          #   };
          # };
          all = pkgs.symlinkJoin {
            name = packageName;
            paths = [ ghc-9-6 ghc-9-8 ];
          };
        };
      });
}
