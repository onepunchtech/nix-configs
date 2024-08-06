{ config, lib, pkgs, ... }:

{
  imports =
    [ 
      ./hardware/bigtux.nix
      ./lib/base.nix
    ];

  services = {
    samba = {
      enable = true;
      securityType = "user";
      extraConfig = ''
        workgroup = WORKGROUP
        server string = smbnix
        netbios name = smbnix
        security = user 
        #use sendfile = yes
        #max protocol = smb2
        hosts allow = 10.10.10.0/24  localhost
        hosts deny = 0.0.0.0/0
        guest account = nobody
        map to guest = bad user
        read raw = Yes
        write raw = Yes
        socket options = TCP_NODELAY IPTOS_LOWDELAY SO_RCVBUF=131072 SO_SNDBUF=131072
        min receivefile size = 16384
        use sendfile = true
        aio read size = 16384
        aio write size = 16384
      '';
      shares = {
        public = {
          path = "/mnt/media/Shares/Whitehead";
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "whitehead";
        };
      };
    };
    jellyfin = {
      enable = true;
      openFirewall = true;
    };

    rsnapshot = {
      enable = true;
      extraConfig = ''
        snapshot_root	/mnt/backups
        no_create_root	1
        retain	hourly	2
        backup	/mnt/media/	localhost/
      '';
      cronIntervals = {
        hourly = "0 * * * *";
      };
    };
  };

  nix = {
    settings = {
      max-jobs = lib.mkDefault 4;
      trusted-users = [ "root" "@wheel" ];
    };

  };

  networking = {
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 80 53 443 ];
    firewall.allowedUDPPorts = [ 53 51000 ];
    hostName = "bigtux";
    networkmanager.enable = true;
    interfaces.enp7s0.useDHCP = true;
  };
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.10.11.2/24" ];
      listenPort = 51000; 

      privateKeyFile = "/root/bigtux.key";

      peers = [

        {
          publicKey = "UvHjyur+U6/dvz+AS/JvmTea5frBk+c/3rmv2Q94RzQ=";

          allowedIPs = [ "10.10.11.0/24" ];
          endpoint = "45.33.120.131:51000"; 
          persistentKeepalive = 25;
        }
      ];
    };
  };
}

