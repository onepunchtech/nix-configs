{ config, pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking = {
    hostName = "ludwig";
    networkmanager.enable = true;
  };

  time.timeZone = "America/Chicago";

  environment.systemPackages = with pkgs; [
    wget 
    vim
    acpitool
    emacs
    git
    tree
    rxvt_unicode
    home-manager
  ];

  services.openssh.enable = true;

  networking.firewall.enable = false;
  services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.enableRedistributableFirmware = true;
  
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "intel" ];

  users.users.whitehead = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };

  system.stateVersion = "19.03";

}
