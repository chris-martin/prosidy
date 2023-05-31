{
  inputs = {
    "nixos-22.11".url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "prosidy";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-22.11" = import inputs."nixos-22.11" { inherit system; };
        };
        pkgs = nixpkgs."nixos-22.11";
        project = pkgs.haskellPackages.developPackage {
          root = ./prosidy;
          name = packageName;
        };
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages = {
          "${packageName}" = project;

          testConfigurations = let

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
            ghc-9-0 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc90";
            };
            ghc-9-2 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc92";
            };
            ghc-9-4 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc94";
              overrides = new: old: {
                aeson =
                  dontCheck (new.callPackage ./nix/aeson-2.1.2.nix { });
                base-compat =
                  dontCheck (new.callPackage ./nix/base-compat-0.13.0.nix { });
                base-compat-batteries = dontCheck
                  (new.callPackage ./nix/base-compat-batteries-0.13.0.nix { });
                foldable1-classes-compat = dontCheck
                  (new.callPackage ./nix/foldable1-classes-compat-0.1.nix { });
                OneTuple =
                  dontCheck (new.callPackage ./nix/OneTuple-0.4.1.nix { });
                quickcheck-instances =
                  dontCheck (new.callPackage ./nix/quickcheck-instances-0.3.29.nix { });
              };
            };
            all = pkgs.symlinkJoin {
              name = packageName;
              paths = [ ghc-9-0 ghc-9-2 ghc-9-4 ];
            };
          };
        };
      });
}
