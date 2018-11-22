{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      concurrent-output = pkgs.haskell.lib.doJailbreak super.concurrent-output;
    };
  };

  natural = modifiedHaskellPackages.callPackage ./natural.nix {};
in
  natural
