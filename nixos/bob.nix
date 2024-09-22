{pkgs, config, ...}:
{
  imports = [
    ./lib/base.nix
    ./hardware/base-hardware.nix
  ];

  networking.hostName = "bob";

  hardware.opengl = {
    enable = true;
  };

  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {

    modesetting.enable = true;

    powerManagement.enable = false;
    powerManagement.finegrained = false;

    open = false;

    nvidiaSettings = true;

    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };
}
