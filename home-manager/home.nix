{ pkgs, emacs-overlay, ... }:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix { };

in
{
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
    transmission_3-gtk
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
    zellij = {
      enable = true;
      settings = {
        pane_frames = false;
        theme = "catppuccin-macchiato";
        keybinds = {
          unbind = [
            "Ctrl g"
            "Ctrl o"
            "Ctrl p"
            "Ctrl n"
            "Ctrl h"
          ];
          normal = {
            "bind \"Alt p\"" = {
              SwitchToMode = "pane";
            };
            "bind \"Alt g\"" = {
              SwitchToMode = "locked";
            };
          };
          pane = {
            "bind \"Alt p\"" = {
              SwitchToMode = "Normal";
            };
          };
          locked = {
            "bind \"Alt g\"" = {
              SwitchToMode = "Normal";
            };
          };
          session = {
            "bind \"Alt o\"" = {
              SwitchToMode = "Normal";
            };
          };
          "shared_except \"session\" \"locked\"" = {
            "bind \"Alt o\"" = {
              SwitchToMode = "Session";
            };
          };
        };
      };
    };
    kitty = {
      enable = true;
      font.name = "DejaVu Sans Mono";
      font.size = 10;
      settings = {
        scrollback_lines = 10000;
        enable_audio_bell = false;
        update_check_interval = 0;
        tab_bar_style = "hidden";
        confirm_os_window_close = 0;
        tab_title_template = "{title}{' :{}:'.format(num_windows) if num_windows > 1 else ''}";
      };
      themeFile = "Catppuccin-Mocha";
      extraConfig = '''';
    };
    nushell = {
      enable = true;
      configFile.source = ./nushell/config.nu;
      envFile.text = ''
        $env.CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense'
        #$env.ZELLIJ_AUTO_ATTACH = true
      '';
      shellAliases = {
        lg = "lazygit";
        z = "zellij -l welcome";
      };
    };
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
      import ./neovim.nix {
        pkgs = pkgs;
        lib = pkgs.lib;
      }
    );
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
  xdg.configFile.nvim.source = ./nvim;

  manual.manpages.enable = false;
}
