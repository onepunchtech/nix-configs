{ pkgs, config, ... }:
{
  imports = [
    ./lib/base.nix
    ./lib/gui.nix
    ./lib/amdgpu.nix
    ./lib/users.nix
    ./lib/shell.nix
    ./lib/sops.nix
    ./lib/printer.nix
    ./lib/networking.nix
    ./lib/virtualization.nix
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

  fileSystems."/mnt/extra2" = {
    device = "/dev/disk/by-label/Extra2";
    fsType = "ext4";
    options = [
      "users"
      "defaults"
      "nofail"
    ];
  };

  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
  services.xserver.videoDrivers = [ "amdgpu" ];
  boot.kernelParams = [
    "video=DP-1:2560x1440@120"
    "video=HDMI-A-1:2560x1440@120"
    "kvm_amd"
  ];

  sops.defaultSopsFile = ./host-secrets/mises-secrets.yaml;
  sops.secrets.whitehead-password = { };
  sops.secrets.whitehead-password.neededForUsers = true;
}
