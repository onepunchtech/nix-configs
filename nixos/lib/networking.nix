{ ... }:
{
  services.resolved.enable = true;

  networking = {
    nameservers = [ "10.10.10.11" ];
    useDHCP = false;
    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
    };
  };
}
