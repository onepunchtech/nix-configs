{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./hardware/bigtux.nix
    ./lib/base.nix
  ];

  environment.systemPackages = with pkgs; [
    tmux
    zellij
  ];

  services = {
    jellyfin = {
      enable = true;
      openFirewall = true;
    };
  };

  nix = {
    settings = {
      max-jobs = lib.mkDefault 4;
      trusted-users = [
        "root"
        "@wheel"
      ];
    };

  };

  networking = {
    firewall.enable = true;
    firewall.allowedTCPPorts = [
      80
      53
      443
    ];
    firewall.allowedUDPPorts = [
      53
      51000
    ];
    hostName = "bigtux";
    networkmanager.enable = true;
    interfaces.enp7s0.useDHCP = true;
  };

  fileSystems."/mnt/nas1/share" = {
    device = "10.10.106.50:/srv/share";
    fsType = "nfs";
    options = [
      "x-systemd.automount"
      "noauto"
      "nfsvers=4.2"
    ];
  };

  fileSystems."/mnt/nas1/repositories" = {
    device = "10.10.106.50:/srv/repositories";
    fsType = "nfs";
    options = [
      "x-systemd.automount"
      "noauto"
      "nfsvers=4.2"
    ];
  };

  fileSystems."/mnt/nas1/media" = {
    device = "10.10.106.50:/srv/media";
    fsType = "nfs";
    options = [
      "x-systemd.automount"
      "noauto"
      "nfsvers=4.2"
    ];
  };

  boot.supportedFilesystems = [ "nfs" ];

  users.users.whitehead = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "video"
      "docker"
      "libvirtd"
      "kvm"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCrOpJm3+B7/pyGi+pzn2HbatpFY7tCDpwBcr8orQOd9B0GXTIuTKeV2lGS9Zb1TUqngo9uR2JXv0o51IZOao0zjGgug2udFvB0mQNALCrEosHVzTGopkeuiF9ZKlaHO5vbzi9zfDWs9/1A1YTa7JFt8Qrgi4EqycOli540jlvvxkEDN3PDz/36YaXCqzqj3e5tX6Nmh8xCEq70+oyA9oZ/gTMLFjLLlSigZPn0Ex3KjRpiap3LkPZGt7LPZEFWXrMMKLhzOhM7yuMewHSiYMp4s6gJursUK3etcxSHn+HeXcMdtte9XRi91PwIhnHR/oqUpP+wNpwm26qRmVeOmn2J YubiKey #26922176 PIV Slot 9a"
    ];
  };

  users.users.root = {
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCrOpJm3+B7/pyGi+pzn2HbatpFY7tCDpwBcr8orQOd9B0GXTIuTKeV2lGS9Zb1TUqngo9uR2JXv0o51IZOao0zjGgug2udFvB0mQNALCrEosHVzTGopkeuiF9ZKlaHO5vbzi9zfDWs9/1A1YTa7JFt8Qrgi4EqycOli540jlvvxkEDN3PDz/36YaXCqzqj3e5tX6Nmh8xCEq70+oyA9oZ/gTMLFjLLlSigZPn0Ex3KjRpiap3LkPZGt7LPZEFWXrMMKLhzOhM7yuMewHSiYMp4s6gJursUK3etcxSHn+HeXcMdtte9XRi91PwIhnHR/oqUpP+wNpwm26qRmVeOmn2J YubiKey #26922176 PIV Slot 9a"
    ];
  };
}
