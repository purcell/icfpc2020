let
  packageName = "icfpc2020";
in
self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      "${packageName}" =
        hself.callCabal2nix
          packageName
          (self.nix-gitignore.gitignoreSourcePure ([ ./.gitignore ]) ./.) { };
    };
  };

  cli = self.haskell.lib.justStaticExecutables (self.haskellPackages."${packageName}");
}
