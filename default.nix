let
  packageName = "icfpc2020";

  # Pinned version of nixpkgs determines our GHC
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = extra:
    pkgs.nix-gitignore.gitignoreSourcePure ([ ./.gitignore ] ++ extra);

  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      "${packageName}" =
        hself.callCabal2nix
          packageName
          (gitignore [ ] ./.) { };
    };
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."${packageName}");

  shell = myHaskellPackages.shellFor {
    packages = p: [ p."${packageName}" ];

    buildInputs = [
      pkgs.nixpkgs-fmt
      pkgs.niv
      myHaskellPackages.hlint
      myHaskellPackages.cabal-install
      myHaskellPackages.ghcid
      myHaskellPackages.ghcide
    ];
    withHoogle = true;
  };

in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  "${packageName}" = myHaskellPackages."${packageName}";
}
