{
  config,
  pkgs,
  ...
}:

{

  imports = [
    ./modules/base.nix
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  home.stateVersion = "25.05";
  home.homeDirectory = "/Users/whitehead";
  home.username = "whitehead";
  home.packages = with pkgs; [
  ];

  #manual.manpages.enable = true;
}
