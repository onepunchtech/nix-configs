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
    dnsutils
    texliveFull
    google-chrome
    transmission_4-gtk
    brightnessctl
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
    brave
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
    kdePackages.polkit-kde-agent-1
    glance
    htop
    wdisplays
    #Language servers
    yaml-language-server
    marksman
    ltex-ls
    vscode-langservers-extracted
    ripgrep
    nodejs
    deno
    nerd-fonts.dejavu-sans-mono
    lm_sensors
    hyprpanel
    # terminal
    lazygit
    lazysql
    ghostty
    # formatters
    tflint
    taplo
    shotcut
    kdePackages.kdenlive
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

  xdg.configFile."waybar/config".source = ./rawConfigs/waybar/waybar.conf;
  xdg.configFile."wofi/style.css".source = ./rawConfigs/wofi/style.css;
  xdg.configFile."waybar/style.css".source = ./rawConfigs/waybar/style.css;
  xdg.configFile."hypr/hyprland.conf".source = ./rawConfigs/hypr/hyprland.conf;
  xdg.configFile."ghostty/config".source = ./rawConfigs/ghostty/config;
  xdg.configFile.nvim.source = ./rawConfigs/nvim;

  manual.manpages.enable = false;
}
