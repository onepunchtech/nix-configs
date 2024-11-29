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
    rofi
    pavucontrol
    pulseaudio
    firefox-wayland
    ltex-ls
    zip
    unzip
    libreoffice
    nixd
    nixpkgs-fmt
    sops
    age
    transmission-gtk
    vlc
    inkscape-with-extensions
    gimp
    blender
    docker-compose
    mako
    polkit-kde-agent
    glances
    htop
    cura
    expressvpn
    yaml-language-server
    nodejs
  ];

  # home.sessionVariables = {
  #   EDITOR = "emc";
  # };

  programs = {
    waybar.enable = true;
    #rofi = (import ./rofi.nix);
    # alacritty = (import ./alacritty.nix);
    git = (import ./git.nix);
    #bash = (import ./bash.nix);
    #autorandr = (import ./autorandr.nix);
    direnv = {
      enable = true;
      enableNushellIntegration = true;
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
    kitty = {
      enable = true;
      font.name = "DejaVu Sans Mono";
      font.size = 10;
      settings = {
        scrollback_lines      = 10000;
        enable_audio_bell     = false;
        update_check_interval = 0;
        tab_bar_min_tabs      = 1;
        tab_bar_edge          = "bottom";
        tab_bar_style         = "powerline";
        tab_powerline_style   = "slanted";
        confirm_os_window_close = 0;
        tab_title_template    = "{title}{' :{}:'.format(num_windows) if num_windows > 1 else ''}";
      };
      theme = "Catppuccin-Mocha";
    };
    nushell = {
      enable = true;
      configFile.source = ./nushell/config.nu;
      # shellAliases = {
      #   vi = "hx";
      #   vim = "hx";
      #   nano = "hx";
      # };
    };
    carapace = {
      enable = true;
      enableNushellIntegration = true;
    };

    starship = {
      enable = true;
      settings = {
        add_newline = true;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
      };
    };
    neovim = (import ./neovim.nix {pkgs = pkgs; lib = pkgs.lib; });
  };


  services = {
    emacs = {
      enable = true;
      socketActivation.enable = true;
      client.enable = true;
    };
  };

  imports = [ ./emacs.nix ];

  xdg.configFile."i3/config".source = ./i3/config;
  xdg.configFile."i3status-rust/config.toml".source = ./i3status-rust/config.toml;
  xdg.configFile."waybar/config".source = ./waybar/waybar.conf;
  xdg.configFile."wofi/style.css".source = ./wofi/style.css;
  xdg.configFile."waybar/style.css".source = ./waybar/style.css;
  xdg.configFile."hypr/hyprland.conf".source = ./hypr/hyprland.conf;

  manual.manpages.enable = false;
}
