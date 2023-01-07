{ pkgs, ... }:


{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkNativeComp.override {
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
      proof-general
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
      graphviz-dot-mode
      which-key
      lsp-pyright
      sbt-mode
      doom-modeline
      dap-mode
      typescript-mode
      all-the-icons
      ledger-mode
      lsp-ltex
      # tree-sitter
      # tree-sitter-langs
      # tree-sitter-langs.withGrammars(g: [g.tree-sitter-rust])
      # tsc
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
