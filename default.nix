{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, deepseq, primitive, semigroups, stdenv
      , tasty, tasty-hunit, vector
      }:
      mkDerivation {
        pname = "nonempty-vector";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base deepseq primitive semigroups vector
        ];
        testHaskellDepends = [ base semigroups tasty tasty-hunit vector ];
        homepage = "https://github.com/emilypi/nonempty-vector";
        description = "Non-empty vectors";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
