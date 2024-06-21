{ config, pkgs, ... }:

let
  unstable = import
    (builtins.fetchTarball https://github.com/nixos/nixpkgs/tarball/master)
    { config = config.nixpkgs.config; };

in
{
  imports =
    [ 
      ./hardware-configuration.nix
    ];

  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      xwayland
      waybar
      mako
      kanshi
    ];
  };

  system.stateVersion = "21.05";

  swapDevices = [ {
    device = "/var/lib/swapfile";
    size = 16*1024;
  } ];
}

