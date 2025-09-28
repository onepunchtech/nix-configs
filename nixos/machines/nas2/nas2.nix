{ pkgs, config, ... }:
{
  imports = [
    ../../lib/base.nix
    ../../lib/shell.nix
    ../../lib/sops.nix
  ];

  networking.hostName = "nas2";

  # optional, but ensures rpc-statsd is running for on demand mounting
  boot.supportedFilesystems = [
    "nfs"
    "zfs"
    "btrfs"
  ];
  boot.zfs.extraPools = [ "storage1" ];

  networking.hostId = "8425e348";

  systemd.network.enable = true;

  systemd.network.networks."10-lan-phys-all" = {
    matchConfig.Name = "enp*";
    networkConfig.DHCP = "ipv4";
  };

  environment.systemPackages = with pkgs; [
    sanoid
  ];

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

  users.users.zfs = {
    shell = pkgs.bash;
    isNormalUser = true;
    extraGroups = [
    ];
  };

  # services.nfs.server = {
  #   enable = true;
  #   lockdPort = 4001;
  #   mountdPort = 4002;
  #   statdPort = 4000;
  #   extraNfsdConfig = '''';
  # };
  #
  # fileSystems."/nfs" = {
  #   device = "/mnt/nas";
  #   options = [ "bind" ];
  # };
  #
  # systemd.tmpfiles.rules = [
  #   "d /nfs 0777 nobody nogroup"
  # ];
  #
  # services.nfs.server.exports = ''
  #   /nfs/share    10.10.100.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #   /nfs/share    10.10.106.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #
  #   /nfs/repositories    10.10.100.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #   /nfs/repositories    10.10.106.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #
  #   /nfs/media    10.10.100.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #   /nfs/media    10.10.106.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #
  #   /nfs/ops-backups    10.10.100.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #   /nfs/ops-backups    10.10.106.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #
  #   /nfs/project-backups    10.10.100.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  #   /nfs/project-backups    10.10.106.0/24(insecure,rw,sync,no_subtree_check,anonuid=1000,anongid=100)
  # '';
  #
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      111
      2049
      4000
      4001
      4002
      20048
      9000 # utility
    ];
    allowedUDPPorts = [
      111
      2049
      4000
      4001
      4002
      20048
      9000 # utility
    ];
  };

  services.sanoid = {
    enable = true;
    templates.backup = {
      autoprune = true;
      autosnap = false;
      frequently = 0;
      hourly = 36;
      daily = 30;
      monthly = 6;
      yearly = 0;
    };

    datasets."storage1/repositories" = {
      useTemplate = [ "backup" ];
    };

    datasets."storage1/ops-backups" = {
      useTemplate = [ "backup" ];
    };

    datasets."storage1/project-backups" = {
      useTemplate = [ "backup" ];
    };

    datasets."storage1/share" = {
      useTemplate = [ "backup" ];
    };

    datasets."storage1/media" = {
      useTemplate = [ "backup" ];
    };
  };

  sops.defaultSopsFile = ../../host-secrets/nas2.yaml;
  sops.secrets."ssh_keys/id_ed25519_zfs" = {
    owner = config.users.users.zfs.name;
    group = config.users.users.zfs.group;
  };

  services.syncoid = {
    enable = true;
    user = "zfs";
    commonArgs = [
      "--no-privilege-elevation"
      "--no-sync-snap"
      "--sshoption=StrictHostKeyChecking=off"
    ];
    sshKey = config.sops.secrets."ssh_keys/id_ed25519_zfs".path;

    commands."media" = {
      source = "zfs@nas1.onepunch:storage1/media";
      target = "storage1/media";
      extraArgs = [ ];

    };

    commands."repositories" = {
      source = "zfs@nas1.onepunch:storage1/repositories";
      target = "storage1/repositories";
      extraArgs = [ ];
    };

    commands."ops-backups" = {
      source = "zfs@nas1.onepunch:storage1/ops-backups";
      target = "storage1/ops-backups";
      extraArgs = [ ];
    };

    commands."project-backups" = {
      source = "zfs@nas1.onepunch:storage1/project-backups";
      target = "storage1/project-backups";
      extraArgs = [ ];
    };

    commands."share" = {
      source = "zfs@nas1.onepunch:storage1/share";
      target = "storage1/share";
      extraArgs = [ ];
    };
  };
}
