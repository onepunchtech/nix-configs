{ pkgs, ... }:

{
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraPackages = epkgs: with epkgs; with melpaPackages; [
      use-package
      evil-numbers
      exec-path-from-shell
      paredit
      flycheck
      flycheck-rust
      idris-mode
      elixir-mode
      ruby-end
      alchemist
      web-mode
      markdown-mode
      yaml-mode
      ghc
      dhall-mode
      js2-mode
      ac-js2
      ag
      f
      let-alist
      s
      elm-mode
      auto-complete
      company
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
      w3m
      rust-mode
      toml-mode
      intero
      floobits
      magit
      forge
      evil-magit
      go-mode
      gotest
      evil-mu4e
      fix-word
      whitespace-cleanup-mode
      indent-guide
      discover-my-major
      proof-general
      nix-mode
      direnv
      solarized-theme
      lsp-mode
      lsp-ui
      lsp-haskell
      psc-ide
      purescript-mode
      treemacs
      lsp-treemacs
      treemacs-evil
      treemacs-projectile
      treemacs-icons-dired
      treemacs-magit
      yasnippet
      restclient
      company-restclient
      graphviz-dot-mode
      which-key
      lsp-python-ms
    ];
  };

  home.file.".emacs.d" = {
    source = ./emacs.d;
    # recursive = true;
    # onChange = ''
    #     rm ~/.emacs.d/init.elc -fv
    #     rm ~/.emacs.d/src/*.elc -fv
    #     emacs -Q -nw -l ~/.emacs.d/init.el -batch -f batch-byte-compile ~/.emacs.d/init.el ~/.emacs.d/src/*.el
    #   '';
  };

  xdg.dataFile."applications/emacsclient.desktop".text = ''
    [Desktop Entry]
    Name=Emacsclient
    GenericName=Text Editor
    Comment=Edit text
    MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
    Exec=emacsclient -c -a emacs %F
    Icon=emacs
    Type=Application
    Terminal=false
    Categories=Development;TextEditor;
    StartupWMClass=Emacs
    Keywords=Text;Editor;
  '';
}
