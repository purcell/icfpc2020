let
  # Pinned version of nixpkgs determines our GHC
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [ (import ./overlay.nix) ];
  };

  shell = pkgs.mkShell {
    name = "icfpc2020";
    buildInputs = with pkgs; [ sbcl nixpkgs-fmt curl wget ];
  };
in
{
  inherit shell;
}
