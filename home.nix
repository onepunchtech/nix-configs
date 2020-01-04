{pkgs, ...}:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in {
  home.username = "whitehead";
  home.packages = with pkgs; [
    google-chrome
    xorg.xbacklight
    i3
    i3status-rust
    stack
    zlib
    xsel
    scripts.emc
    scripts.opdt
    ag
    (all-hies.selection { selector = p: { inherit (p) ghc865 ghc864; }; })
    (python3.withPackages(ps: [
      ps.python-language-server
      ps.pyls-mypy ps.pyls-isort ps.pyls-black
    ]))
  ];

  home.sessionVariables = {
    EDITOR = "emc";
  };

  programs = {
    firefox = {
      enable = true;
    };
    rofi = (import ./rofi.nix);
    alacritty = (import ./alacritty.nix);
    # emacs = (import ./emacs.nix);
    git = (import ./git.nix);
    bash = (import ./bash.nix);
    autorandr = (import ./autorandr.nix);
  };

  imports = [ ./emacs.nix ];

  xdg.configFile."keyboard/config.xkb".source = ./keyboard/config.xkb;
  xsession.enable = true;
  xsession.initExtra = "xkbcomp ~/.config/keyboard/config.xkb $DISPLAY";
  xsession.windowManager.command = "${pkgs.i3}/bin/i3";

  xdg.configFile."i3/config".source = ./i3/config;
  xdg.configFile."i3status-rust/config.toml".source = ./i3status-rust/config.toml;
  xdg.configFile."brittany/config.yaml".source = ./brittany/config.yaml;

  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
}
