{pkgs, ...}:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix {};
  # oldemacsRev = "e9fe554910fabf301ebf244aa2f9e5f2d5f69c4b";
  emacsRev = "2891c0c12ad28fb24eafbce9a273553d3ef94def";
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
    silver-searcher
    alacritty
    wofi
    pavucontrol
    pulseaudio
    firefox-wayland
    ltex-ls
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
  };

  services = {
    lorri.enable = true;
    emacs = {
      enable = true;
      socketActivation.enable = true;
      client.enable = true;
      defaultEditor = true;
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
