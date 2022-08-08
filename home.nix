{pkgs, ...}:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix {};
  emacsRev = "e9fe554910fabf301ebf244aa2f9e5f2d5f69c4b";
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
    silver-searcher
    alacritty
    wofi
    pavucontrol
    pulseaudio
    firefox-wayland
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
}
