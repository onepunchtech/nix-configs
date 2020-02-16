{ pkgs }:

let
  rev = "26c8e4d13b7c8e1e44264e92fe9ea28be1850580";
  emacs-overlay = import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
  });
in
{
  config = {
    allowUnfree = true;
    allowBroken = true;
  };
  overlays = [ emacs-overlay ];
}
