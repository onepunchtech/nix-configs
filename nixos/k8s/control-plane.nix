{ pkgs, ... }:

{

  imports = [
    ../lib/base.nix
    ../lib/users.nix
    ../lib/shell.nix
    ../lib/sops.nix
  ];

  networking = {
    useDHCP = false;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        53
        3000
      ];
      allowedUDPPorts = [
        53
      ];
    };
    networkmanager.enable = false;

  };

  virtualisation.docker.enable = true;

  sops.secrets.whitehead-password = { };
  sops.secrets.whitehead-password.neededForUsers = true;
}
