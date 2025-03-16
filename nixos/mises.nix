{ pkgs, config, ... }:
{
  imports = [
    ./lib/base.nix
    ./lib/gui.nix
    ./lib/amdgpu.nix
    ./hardware/base-hardware.nix
  ];

  networking.hostName = "mises";

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = [
      pkgs.amdvlk
    ];
  };

  # fileSystems."/storage1" = {
  #   device = "/dev/disk/by-label/Storage1";
  #   fsType = "ext4";
  #   options = [
  #     "users"
  #     "defaults"
  #     "nofail"
  #   ];
  # };
  #
  # fileSystems."/extra1" = {
  #   device = "/dev/disk/by-label/Extra1";
  #   fsType = "ext4";
  #   options = [
  #     "users"
  #     "defaults"
  #     "nofail"
  #   ];
  # };
  #
  # fileSystems."/mnt/extra2" = {
  #   device = "/dev/disk/by-label/Extra2";
  #   fsType = "ext4";
  #   options = [
  #     "users"
  #     "defaults"
  #     "nofail"
  #   ];
  # };

  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
  services.xserver.videoDrivers = [ "amdgpu" ];
  boot.kernelParams = [
    "video=DP-1:2560x1440@120"
    "video=HDMI-A-1:2560x1440@120"
    "kvm_amd"
  ];
}
