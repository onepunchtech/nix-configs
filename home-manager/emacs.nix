{ pkgs, ... }:


{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs.override {
      imagemagick = pkgs.imagemagickBig;
    };


    extraPackages = epkgs: with epkgs; with melpaPackages; [
      pdf-tools
      latex-extra
      use-package
      evil-numbers
      exec-path-from-shell
      paredit
      racket-mode
      on-parens
      evil-surround
      flycheck
      flycheck-rust
      idris-mode
      elixir-mode
      ruby-end
      alchemist
      web-mode
      markdown-mode
      yaml-mode
      dhall-mode
      js2-mode
      ac-js2
      ag
      f
      let-alist
      s
      elm-mode
      autothemer
      json-reformat
      json-mode
      writegood-mode
      projectile
      ag
      helm
      helm-projectile
      helm-ag
      helm-flx
      helm-fuzzier
      helm-tramp
      helm-lsp
      helm-xref
      w3m
      rustic
      rust-mode
      toml-mode
      cargo
      floobits
      magit
      forge
      evil-collection
      haskell-mode
      go-mode
      gotest
      fix-word
      whitespace-cleanup-mode
      indent-guide
      discover-my-major
      nix-mode
      direnv
      gruvbox-theme
      lsp-mode
      lsp-ui
      lsp-haskell
      psc-ide
      purescript-mode
      treemacs
      treemacs-nerd-icons
      lsp-treemacs
      treemacs-evil
      treemacs-projectile
      treemacs-icons-dired
      treemacs-magit
      yasnippet
      yasnippet-snippets
      restclient
      restart-emacs
      graphviz-dot-mode
      which-key
      lsp-pyright
      doom-modeline
      typescript-mode
      nerd-icons
      ledger-mode
      lsp-ltex
      terraform-mode
      org-present
      org-superstar
      visual-fill-column
      scala-mode
      sbt-mode
      dap-mode
      posframe
      lsp-java
      ox-reveal
      dash
      company-tabnine
      tabnine
    ];
  };

  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
    onChange = ''
      rm ~/.emacs.d/init.elc -fv
      rm ~/.emacs.d/config/*.elc -fv
      #emacs -Q -nw -l ~/.emacs.d/init.el -batch -f batch-byte-compile ~/.emacs.d/init.el
    '';
  };
}
