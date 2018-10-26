{ compiler ? "ghc861" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              keywee = pkgs.haskell.lib.dontHaddock
                (
                  haskellPackagesNew.callPackage ./default.nix { }
                );
              reactive-banana = pkgs.haskell.lib.doJailbreak
                haskellPackagesOld.reactive-banana;
            };   
          };
        };
      };
    }; 
  };

  pkgs = import <unstable> { inherit config; };
  
in
  { keywee = pkgs.haskell.packages.${compiler}.keywee;
  }
