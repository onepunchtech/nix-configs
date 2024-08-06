{ pkgs, config, ... }:

let
  wan = "enp1s0";
  lan = "enp2s0";

in

{
  imports = [
    ./hardware/router.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  settings.experimental-features = [ "nix-command" "flakes" ];

  time.timeZone = "US/Mountain";

  networking = {
    useDHCP = false;
    firewall.enable = false;
    hostName = "router";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    nat.enable = false;
    nftables = {
      enable = true;
      ruleset = ''
        table ip filter {
          chain output {
            type filter hook output priority 100; policy accept;
          }

          chain input {
            type filter hook input priority filter; policy accept;
            iifname "${lan}" counter accept
            iifname "${wan}" ct state { established, related } counter accept
            iifname "${wan}" drop
          }

          chain forward {
            type filter hook forward priority filter; policy drop;
            iifname "${lan}" oifname "${wan}" counter accept comment "Allow trusted LAN to WAN"
            iifname "${wan}" oifname "${lan}" ct state related,established counter accept comment "Allow established back to LANs"
          }
        }

        table ip nat {
          chain prerouting {
            type nat hook output priority filter; policy accept;
          }

          chain postrouting {
            type nat hook postrouting priority filter; policy accept;
            oifname "${wan}" masquerade
          }
        }
      '';
    };

    interfaces.enp1s0.useDHCP = true;
    interfaces.enp2s0 = {
      useDHCP = false;
      ipv4.addresses = [{ address = "10.10.10.1"; prefixLength = 24; }];
    };
    interfaces.enp3s0.useDHCP = false;
    interfaces.enp4s0.useDHCP = false;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "latarcyrheb-sun32";
    keyMap = "us";
  };

  users.users.protectli = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCrOpJm3+B7/pyGi+pzn2HbatpFY7tCDpwBcr8orQOd9B0GXTIuTKeV2lGS9Zb1TUqngo9uR2JXv0o51IZOao0zjGgug2udFvB0mQNALCrEosHVzTGopkeuiF9ZKlaHO5vbzi9zfDWs9/1A1YTa7JFt8Qrgi4EqycOli540jlvvxkEDN3PDz/36YaXCqzqj3e5tX6Nmh8xCEq70+oyA9oZ/gTMLFjLLlSigZPn0Ex3KjRpiap3LkPZGt7LPZEFWXrMMKLhzOhM7yuMewHSiYMp4s6gJursUK3etcxSHn+HeXcMdtte9XRi91PwIhnHR/oqUpP+wNpwm26qRmVeOmn2J YubiKey #26922176 PIV Slot 9a"
    ];
  };

  users.users.root = {
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCrOpJm3+B7/pyGi+pzn2HbatpFY7tCDpwBcr8orQOd9B0GXTIuTKeV2lGS9Zb1TUqngo9uR2JXv0o51IZOao0zjGgug2udFvB0mQNALCrEosHVzTGopkeuiF9ZKlaHO5vbzi9zfDWs9/1A1YTa7JFt8Qrgi4EqycOli540jlvvxkEDN3PDz/36YaXCqzqj3e5tX6Nmh8xCEq70+oyA9oZ/gTMLFjLLlSigZPn0Ex3KjRpiap3LkPZGt7LPZEFWXrMMKLhzOhM7yuMewHSiYMp4s6gJursUK3etcxSHn+HeXcMdtte9XRi91PwIhnHR/oqUpP+wNpwm26qRmVeOmn2J YubiKey #26922176 PIV Slot 9a"
    ];
  };

  environment.systemPackages = with pkgs; [
    htop
    vim
    wget
  ];


  services = {
    monit.enable = true;
    openssh = {
      enable = true;
      ports = [ 1122 ];
    };
    dhcpd4 = {
      enable = true;
      interfaces = [ "${lan}" ];
      extraConfig = ''
        option domain-name-servers 8.8.8.8, 8.8.4.4;
        option subnet-mask 255.255.255.0;

        subnet 10.10.10.0 netmask 255.255.255.0 {
          option broadcast-address 10.10.10.255;
          option routers 10.10.10.1;
          interface ${lan};
          range 10.10.10.100 10.10.10.254;

          host switch1 {
            hardware ethernet e0:1c:fc:aa:8f:24;
            fixed-address 10.10.10.2;
          }

          host tplinkaccesspointoffice {
            hardware ethernet 00:5f:67:72:28:f6;
            fixed-address 10.10.10.3;
          }

          host nighthawk {
            hardware ethernet a0:63:91:4b:41:56;
            fixed-address 10.10.10.4;
          }

          host bigtux {
            hardware ethernet 40:8d:5c:51:62:b6;
            fixed-address 10.10.10.5;
          }

          host outdooraccesspoint {
            hardware ethernet 00:5f:67:ad:4f:b6;
            fixed-address 10.10.10.6;
          }

          host camerafrontdoor {
            hardware ethernet d4:21:22:89:6e:df;
            fixed-address 10.10.10.7;
          }

          host backupnas {
            hardware ethernet 6c:bf:b5:02:3d:a6;
            fixed-address 10.10.10.8;
          }

          host tplinkaccesspointserverroom {
            hardware ethernet 00:5f:67:72:18:da;
            fixed-address 10.10.10.9;
          }

          host accontroller {
            hardware ethernet 02:42:74:c9:7a:69;
            fixed-address 10.10.10.30;
          }

          {
            hardware ethernet 82:eb:3a:dd:27:e9;
            fixed-address 10.10.10.31;
          }

        }

      '';
    };
  };


  system.stateVersion = "21.05"; # Did you read the comment?

}
