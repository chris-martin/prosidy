let
    pkgs = 
        import <nixpkgs> {};

    haskellPackages =
        pkgs.haskell.packages.ghc881.override {
            overrides = self: super: {
                lens = pkgs.haskell.lib.overrideCabal super.lens (old:
                    {
                        version = "4.18.1";
                        revision = null;
                        editedCabalFile = null;
                        patches = [];
                        libraryHaskellDepends = old.libraryHaskellDepends ++ [self.type-equality];
                        sha256 = "1lmxjaj32v06l12gy00rpjp2lk1cblh3k7kwklk655ss2vas61ri";
                    }
                );
            };
        };

    rules = self: {
        env = pkgs.buildEnv {
	        name = "prosidy-env";
	        paths = [
                pkgs.binutils.bintools
                pkgs.fswatch
                self.ghc
                pkgs.patchelf
	        ];
	    };

        ghc = haskellPackages.ghcWithPackages (hs: with hs; [
            aeson
            aeson-diff
            aeson-pretty
            blaze-html
            cabal-install
            generic-lens
            lens
            mmorph
            megaparsec
            optparse-applicative
            shake
	        skylighting-core
            tasty
            tasty-golden
            tasty-hunit
            wai-app-static
        ]);
    };
in
    pkgs.lib.fix rules
