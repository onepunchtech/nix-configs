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
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

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
    openssh = {
      enable = true;
    };

    kea.dhcp4 = {
      enable = true;
      settings = {
        valid-lifetime = 4000;
        rebind-timer = 2000;
        renew-timer = 1000;

        interfaces-config = {
          interfaces = [
            "${lan}"
          ];
        };

        lease-database = {
          name = "/var/lib/kea/dhcp4.leases";
          persist = true;
          type = "memfile";
        };

        subnet4 = [
          {
            pools = [
              {
                pool = "10.10.10.100 - 10.10.10.254";
              }
            ];
            subnet = "10.10.10.0/24";
            reservations = [
              {
                hw-address = "e0:1c:fc:aa:8f:24";
                ip-address = "10.10.10.2";
                hostname = "switch.lan.";
              }
              {
                hw-address = "00:5f:67:72:28:f6";
                ip-address = "10.10.10.3";
                hostname = "officeaccesspoint.lan.";
              }
              {
                hw-address = "a0:63:91:4b:41:56";
                ip-address = "10.10.10.4";
                hostname = "nighthawk.lan.";
              }
              {
                hw-address = "40:8d:5c:51:62:b6";
                ip-address = "10.10.10.5";
                hostname = "bigtux.lan.";
              }
              {
                hw-address = "00:5f:67:ad:4f:b6";
                ip-address = "10.10.10.6";
                hostname = "outdooraccesspoint.lan.";
              }
              {
                hw-address = "d4:21:22:89:6e:df";
                ip-address = "10.10.10.7";
                hostname = "camerafrontdoor.lan.";
              }
              {
                hw-address = "6c:bf:b5:02:3d:a6";
                ip-address = "10.10.10.8";
                hostname = "backupnas.lan.";
              }
              {
                hw-address = "00:5f:67:72:18:da";
                ip-address = "10.10.10.9";
                hostname = "waterclosetaccesspoint.lan.";
              }
              {
                hw-address = "02:42:74:c9:7a:69";
                ip-address = "10.10.10.30";
                hostname = "accontroller.lan.";
              }
              {
                hw-address = "82:eb:3a:dd:27:e9";
                ip-address = "10.10.10.31";
                hostname = "3dprinter.lan.";
              }
            ];
          }
        ];
        # dhcp-ddns = {
        #   enable-updates = true;
        # };
        option-data =  [
          {
            name = "domain-name-servers";
            data = "10.10.10.1";
          }
          {
            name = "routers";
            data = "10.10.10.1";
          }
          {
            name = "subnet-mask";
            data = "255.255.255.0";
          }
          {
            name = "broadcast-address";
            data = "10.10.10.255";
          }
        ];
      };
    };
    unbound = {
      enable = true;
      settings = {
        server = {
          interface = [ "127.0.0.1" ];
          port = 5335;
          access-control = [ "127.0.0.1 allow" ];
          harden-glue = true;
          harden-dnssec-stripped = true;
          use-caps-for-id = false;
          prefetch = true;
          edns-buffer-size = 1232;

          hide-identity = true;
          hide-version = true;

          local-data = [
            ''"router.lan.  IN A 10.10.10.1"''
            ''"switch.lan.  IN A 10.10.10.2"''
            ''"bigtux.lan.  IN A 10.10.10.5"''
            ''"camerafrontdoor.lan.  IN A 10.10.10.7"''
            ''"backupnas.lan.  IN A 10.10.10.8"''
            ''"accontroller.lan.  IN A 10.10.10.30"''
            ''"3dprinter.lan.  IN A 10.10.10.31"''
          ];
        };
        forward-zone = [
          {
            name = ".";
            forward-addr = [
              "1.1.1.3"
              "1.0.0.3"
            ];
            #forward-tls-upstream = true;  # Protected DNS
          }
        ];
        # local-zone = [''"lan." static''];
      };
    };

    adguardhome = {
      enable = true;
      settings = {
        http = {
          address = "127.0.0.1:3003";
        };
        dns = {
          upstream_dns = [
            "127.0.0.1:5335"
          ];
        };
        filtering = {
          protection_enabled = true;
          filtering_enabled = true;

          parental_enabled = true;
          safe_search = {
            enabled = false;
          };
        };
        filters = map(url: { enabled = true; url = url; }) [
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_9.txt"
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_11.txt"
          "https://raw.githubusercontent.com/AdguardTeam/FiltersRegistry/master/filters/filter_17_TrackParam/filter.txt"
        ];
      };
    };
  };

  system.stateVersion = "21.05";

}
