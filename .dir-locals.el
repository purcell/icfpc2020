;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
  (eglot-server-programs
   (haskell-mode "ghcide" "--lsp"))
  (mode . ormolu-on-save))
 (nix-mode
  (mode . nixpkgs-fmt-on-save)))

