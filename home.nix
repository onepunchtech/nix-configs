{pkgs, ...}:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix {};
  emacsRev = "709cd326377dc8ac1c149a6f29449cdb8322b3c1";
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
  };

  services = {
    lorri.enable = true;
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
