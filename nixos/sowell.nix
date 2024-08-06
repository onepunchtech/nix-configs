{pkgs, ...}:
{
  imports = [
    ./hardware/base-hardware.nix
    ./lib/base.nix
    ./lib/nvidiagpu.nix
  ];
  networking.hostName = "sowell";
  boot.initrd.kernelModules = ["snd_pci_acp6x"];
  hardware.enableAllFirmware = true;
  hardware.firmware = [
    pkgs.firmwareLinuxNonfree
  ];
}
