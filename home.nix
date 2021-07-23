{pkgs, ...}:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix {};
  emacsRev = "b678b3774a84b5e96554e26dc0aa2bb4461178ee";
  emacs-overlay = import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/${emacsRev}.tar.gz";
  });

in {
  nixpkgs = {
    config = import ./nixpkgs-config.nix;
    overlays = [ emacs-overlay ];
  };

  home.username = "whitehead";
  home.packages = with pkgs; [
    google-chrome
    brightnessctl
    i3
    i3status-rust
    zlib
    xsel
    scripts.emc
    scripts.opdt
    imagemagick
    dhall
    dhall-json
    dhall-lsp-server
    ag
    (python3.withPackages(ps: [
      ps.python-language-server
      ps.pyls-mypy ps.pyls-isort ps.pyls-black
    ]))
  ];

  home.sessionVariables = {
    EDITOR = "emc";
  };

  programs = {
    rofi = (import ./rofi.nix);
    alacritty = (import ./alacritty.nix);
    git = (import ./git.nix);
    bash = (import ./bash.nix);
    autorandr = (import ./autorandr.nix);
    direnv = {
      enable = true;
      enableBashIntegration = true;
    };
    ssh.forwardAgent = true;
    tmux = {
      enable = true;
      keyMode = "vi";
      historyLimit = 10000;
      shortcut = "a";
      extraConfig = ''
        set -g mouse on
      '';
    };
  };

  services = {
    lorri.enable = true;
    emacs = {
      enable = true;
      socketActivation.enable = true;
      client.enable = true;
    };
  };

  imports = [ ./emacs.nix ];

  # xdg.configFile."keyboard/config.xkb".source = ./keyboard/config.xkb;
  xsession.enable = true;
  # xsession.initExtra = "xkbcomp ~/.config/keyboard/config.xkb $DISPLAY";
  xsession.windowManager.command = "${pkgs.i3}/bin/i3";

  xdg.configFile."i3/config".source = ./i3/config;
  xdg.configFile."i3status-rust/config.toml".source = ./i3status-rust/config.toml;
  xdg.configFile."brittany/config.yaml".source = ./brittany/config.yaml;

  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
}
