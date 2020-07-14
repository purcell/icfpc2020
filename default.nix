let
  # Pinned version of nixpkgs determines our GHC
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [ (import ./overlay.nix) ];
  };

  shell = with pkgs; haskellPackages.shellFor {
    packages = p: [ p.icfpc2020 ];

    buildInputs = [
      nixpkgs-fmt
      niv
      haskellPackages.hlint
      haskellPackages.ormolu
      haskellPackages.cabal-install
      haskellPackages.ghcid
      haskellPackages.ghcide
    ];
    withHoogle = true;
  };
in
{
  inherit shell;
  inherit (pkgs) cli;
}
