{
  config,
  pkgs,
  extra,
  ...
}:

{

  imports = [
    # ./programs/emacs.nix
    extra.hyprpanel.homeManagerModules.hyprpanel
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      permittedInsecurePackages = [
        "openssl-1.0.2u"
      ];
    };
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
    imagemagick
    dhall
    dhall-json
    semgrep
    silver-searcher
    wofi
    pavucontrol
    pulseaudio
    firefox
    ltex-ls
    zip
    unzip
    libreoffice
    nixd
    nixpkgs-fmt
    sops
    age
    vlc
    inkscape-with-extensions
    gimp
    blender
    docker-compose
    mako
    polkit-kde-agent
    glance
    htop
    expressvpn
    yaml-language-server
    ripgrep
    nodejs
    nerd-fonts.dejavu-sans-mono
    lazygit
    lm_sensors
    hyprpanel
  ];

  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    name = "Nordzy-cursors";
    package = pkgs.nordzy-cursor-theme;
    size = 24;
  };

  gtk.iconTheme = {
    name = "Nordzy";
    package = pkgs.nordzy-icon-theme;
  };

  # home.sessionVariables = {
  #   EDITOR = "emc";
  # };

  programs = {
    waybar.enable = true;
    git = (import ./programs/git.nix);
    direnv = {
      enable = true;
      enableNushellIntegration = false;
      nix-direnv.enable = true;
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
    hyprpanel = (import ./programs/hyprpanel.nix);
    zellij = (import ./programs/zellij.nix);
    kitty = (import ./programs/kitty.nix);
    nushell = (import ./programs/nushell.nix);
    carapace = {
      enable = true;
      enableNushellIntegration = true;
    };

    starship = {
      enable = true;
      enableNushellIntegration = true;
      settings = {
        add_newline = true;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
      };
    };
    neovim = (
      import ./programs/neovim.nix {
        pkgs = pkgs;
        lib = pkgs.lib;
      }
    );
  };

  services = {
    hypridle.enable = true;
    # emacs = {
    #   enable = true;
    #   socketActivation.enable = true;
    #   client.enable = true;
    # };
  };

  xdg.configFile."i3/config".source = ./rawConfigs/i3/config;
  xdg.configFile."i3status-rust/config.toml".source = ./rawConfigs/i3status-rust/config.toml;
  xdg.configFile."waybar/config".source = ./rawConfigs/waybar/waybar.conf;
  xdg.configFile."wofi/style.css".source = ./rawConfigs/wofi/style.css;
  xdg.configFile."waybar/style.css".source = ./rawConfigs/waybar/style.css;
  xdg.configFile."hypr/hyprland.conf".source = ./rawConfigs/hypr/hyprland.conf;
  xdg.configFile.nvim.source = ./rawConfigs/nvim;

  manual.manpages.enable = false;
}
