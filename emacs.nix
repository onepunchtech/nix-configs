{ pkgs, ... }:


{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs.override {
      imagemagick = pkgs.imagemagickBig;
    };


    extraPackages = epkgs: with epkgs; with melpaPackages; [
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
      treemacs-all-the-icons
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
      all-the-icons
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
      foo
      lsp-java
      ox-reveal
      # tree-sitter
      # tree-sitter-langs
      # tree-sitter-langs.withGrammars(g: [g.tree-sitter-rust])
      # tsc
    ];

    overrides = self: super: rec {
      foo = self.melpaPackages.lsp-metals.overrideAttrs(old: {
        src  = pkgs.fetchFromGitHub {
          owner = "prashantvithani";
          repo = "lsp-metals";
          rev = "a2df7263ece6ac69214e41c52d66aab8d3f650eb";
          sha256 = "sha256-37MU9tGRrbD8SyCW/u/WXIMnJE9cOGS4BMioA58JLvo=";

        };
      });
    };
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

  # xdg.dataFile."applications/emacsclient.desktop".text = ''
  #   [Desktop Entry]
  #   Name=Emacsclient
  #   GenericName=Text Editor
  #   Comment=Edit text
  #   MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
  #   Exec=emacsclient -c -a emacs %F
  #   Icon=emacs
  #   Type=Application
  #   Terminal=false
  #   Categories=Development;TextEditor;
  #   StartupWMClass=Emacs
  #   Keywords=Text;Editor;
  # '';
}
