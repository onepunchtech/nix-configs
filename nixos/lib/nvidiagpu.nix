{ config, ... }:
{
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
  };

  hardware.graphics.driSupport32Bit = true;

  services.xserver.videoDrivers = [ "nvidia" ];

}
