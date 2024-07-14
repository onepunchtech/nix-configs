{pkgs, ...}:
{
  imports = [
    ./base.nix
    ./base-hardware.nix
    ./nvidiagpu.nix
  ];
  networking.hostName = "sowell";
  boot.initrd.kernelModules = ["snd_pci_acp6x"];
  hardware.enableAllFirmware = true;
  hardware.firmware = [
    pkgs.firmwareLinuxNonfree
  ];
}
