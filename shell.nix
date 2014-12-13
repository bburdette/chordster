let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        chordster = self.callPackage ./. {};
      };
    };
 in pkgs.lib.overrideDerivation haskellPackages.chordster (attrs: {
   buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })
