{...}:
{
  imports = [
    ./base.nix
    ./base-hardware.nix
    ./nvidiagpu.nix
  ];

  networking.hostName = "mises";
}
