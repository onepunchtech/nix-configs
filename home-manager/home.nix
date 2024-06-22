{pkgs, emacs-overlay, ...}:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix {};

in {
  nixpkgs = {
    config = import ./nixpkgs-config.nix;
    overlays = [ emacs-overlay.overlays.default ];
  };

  home.stateVersion = "24.05";
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
    #scripts.opdt
    imagemagick
    dhall
    dhall-json
    semgrep
    silver-searcher
    alacritty
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
    waybar.enable = true;
    #rofi = (import ./rofi.nix);
    alacritty = (import ./alacritty.nix);
    git = (import ./git.nix);
    bash = (import ./bash.nix);
    #autorandr = (import ./autorandr.nix);
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
    emacs = {
      enable = true;
      socketActivation.enable = true;
      client.enable = true;
    };
  };

  imports = [ ./emacs.nix ];

  xdg.configFile."waybar/config".source = ./sway/waybar.conf;
  xdg.configFile."waybar/style.css".source = ./sway/style.css;
  xdg.configFile."hypr/hyprland.conf".source = ./hypr/hyprland.conf;

  manual.manpages.enable = false;
}
