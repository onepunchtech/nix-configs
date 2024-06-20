{pkgs, ...}:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix {};
  # oldemacsRev = "d7eeebd439b52b77958eb3d8043f3262701ddee2";
  emacsRev = "f955f65318caf28da81fe80f5437c6446dbfa7a3";
  emacs-overlay = import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/${emacsRev}.tar.gz";
  });

in {
  nixpkgs = {
    config = import ./nixpkgs-config.nix;
    overlays = [ emacs-overlay ];

  };

  home.stateVersion = "22.11";
  home.homeDirectory = "/home/whitehead";
  home.username = "whitehead";
  home.packages = with pkgs; [
    texliveFull
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
    semgrep
    nodejs_21
    # dhall-lsp-server
    silver-searcher
    alacritty
    #stack
    #haskell-language-server
    wofi
    pavucontrol
    pulseaudio
    firefox-wayland
    ltex-ls
    retroarchFull
    zip
    unzip
    libreoffice
    nixd
    nixpkgs-fmt
  ];

  home.sessionVariables = {
    EDITOR = "emc";
  };

  programs = {
    #rofi = (import ./rofi.nix);
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
    obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
      ];
    };
  };

  services = {
    emacs = {
      enable = true;
      socketActivation.enable = true;
      client.enable = true;
    };
    kanshi = {
      enable = true;
      profiles = (import ./sway/kanshi.nix);
    };
  };

  imports = [ ./emacs.nix ];

  xdg.configFile."i3/config".source = ./i3/config;
  xdg.configFile."i3status-rust/config.toml".source = ./i3status-rust/config.toml;
  xdg.configFile."brittany/config.yaml".source = ./brittany/config.yaml;
  xdg.configFile."waybar/config".source = ./sway/waybar.conf;
  # xdg.configFile."kanshi/config".source = ./sway/kanshi.conf;
  xdg.configFile."waybar/style.css".source = ./sway/style.css;
  xdg.configFile."opdt/config.dhall".source = ./opdt/config.dhall;

  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  wayland.windowManager.sway = (with pkgs; import ./sway/sway.nix {inherit alacritty wofi waybar;});
  manual.manpages.enable = false;
}
