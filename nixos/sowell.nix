{ pkgs, ... }:
{
  imports = [
    ./lib/base.nix
    ./hardware/base-hardware.nix
    ./lib/gui.nix
    ./lib/nvidiagpu.nix
    ./lib/laptop.nix
    ./lib/driver.nix
  ];
  networking.hostName = "sowell";
  boot.initrd.kernelModules = [
    "nvidia"
    "nvidia_drm"
    "nvidia_uvm"
    "nvidia_modeset"
    "snd_pci_acp6x"
  ];

  hardware.enableAllFirmware = true;
  hardware.firmware = [
    pkgs.firmwareLinuxNonfree
  ];
  swapDevices = [
    {
      device = "/swapfile";
      size = 32 * 1024;
    }
  ];
  services.power-profiles-daemon.enable = false;
}
