{ compiler ? "ghc863" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
	      hie = (import (fetchTarball {
	        url = https://github.com/domenkozar/hie-nix/tarball/master/domenkozar-hie-nix-6794005.tar.gz;
		sha256 = "0pc90ns0xcsa6b630d8kkq5zg8yzszbgd7qmnylkqpa0l58zvnpn";
              }) {}).hie86;
              keywee = pkgs.haskell.lib.dontHaddock
                (
                  haskellPackagesNew.callPackage ./default.nix { }
                );
              reactive-banana = pkgs.haskell.lib.doJailbreak
                haskellPackagesOld.reactive-banana;
              brittany = pkgs.haskell.lib.doJailbreak
	        haskellPackagesOld.brittany;
              multistate = pkgs.haskell.lib.doJailbreak
	        haskellPackagesOld.multistate;
              hsimport = pkgs.haskell.lib.doJailbreak
	        haskellPackagesOld.hsimport;
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
