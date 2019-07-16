{pkgs, ...}:

let
  scripts = pkgs.callPackage ./scripts/scripts.nix {};
in {
  home.username = "whitehead";
  home.packages = with pkgs; [
    google-chrome
    xorg.xbacklight
    i3
    i3status
    scripts.pavol
    scripts.emc
  ];
  home.sessionVariables = {
    GDK_SCALE = 2;
  };

  programs = {
    firefox = {
      enable = true;
    };
    rofi = (import ./rofi.nix);
    alacritty = (import ./alacritty.nix);
    emacs = (import ./emacs.nix);
  };

  services.emacs.enable = true;

  xdg.configFile."keyboard/config.xkb".source = ./keyboard/config.xkb;
  xsession.enable = true;
  xsession.initExtra = "xkbcomp ~/.config/keyboard/config.xkb $DISPLAY";
  xsession.windowManager.command = "${pkgs.i3}/bin/i3";
  home.file.".emacs.d".source = ./emacs.d;

  xdg.configFile."i3/config".source = ./i3/config;
  xdg.configFile."i3status/config".source = ./i3status/config;

  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source =
      ./nixpkgs-config.nix;
}
