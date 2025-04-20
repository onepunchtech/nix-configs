{
  pkgs,
  config,
  lib,
  ...
}:

{

  imports = [
    ../lib/base.nix
    ../lib/users.nix
    ../lib/shell.nix
    ../lib/sops.nix
  ];

  options = {
    options.services.unbound.settings.server.local-data = lib.mkOption {
      type = lib.types.anything;
    };

    options.services.unbound.settings.server.local-zone = lib.mkOption {
      type = lib.types.anything;
    };

  };
  config =
    let
      wan = "enp1s0";

      lan1 = {
        iface = "enp2s0";
        gw = "10.10.51.1";
        ip = "10.10.51.1/24";
        subnet = "10.10.51.0/24";
      };

      lan2 = {
        iface = "enp3s0";
        gw = "10.10.53.1";
        ip = "10.10.53.1/24";
        subnet = "10.10.53.0/24";
      };

      lan3 = {
        iface = "enp4s0";
        gw = "10.10.55.1";
        ip = "10.10.55.1/24";
        subnet = "10.10.55.0/24";
      };
    in
    {

      boot.kernel.sysctl = {
        "net.ipv4.conf.all.forwarding" = true;
        "net.ipv6.conf.all.forwarding" = true;
      };

      systemd.network = {
        wait-online.anyInterface = true;
        networks = {
          "10-wan" = {
            matchConfig.Name = wan;
            networkConfig = {
              DHCP = "ipv4";
              IPv4Forwarding = true;
              IPMasquerade = "ipv4";
            };
          };
          "10-lan1" = {
            matchConfig.Name = lan1.iface;
            address = [ lan1.ip ];
            linkConfig.RequiredForOnline = "no";
            networkConfig = {
              ConfigureWithoutCarrier = true;
            };
          };
          "10-lan2" = {
            matchConfig.Name = lan2.iface;
            address = [ lan2.ip ];
            linkConfig.RequiredForOnline = "no";
            networkConfig = {
              ConfigureWithoutCarrier = true;
            };
          };
          "10-lan3" = {
            matchConfig.Name = lan3.iface;
            address = [ lan3.ip ];
            linkConfig.RequiredForOnline = "no";
            networkConfig = {
              ConfigureWithoutCarrier = true;
            };
          };
        };
      };

      networking = {
        hostName = "router";
        useNetworkd = true;
        useDHCP = false;

        nat.enable = false;
        firewall.enable = false;

        nftables = {
          enable = true;
          ruleset = ''
            table inet filter {

              #flowtable f {
                #hook ingress priority 0;
                #devices = { ${wan}, ${lan1.iface}, ${lan2.iface}, ${lan3.iface} };
              #}

              chain output {
                type filter hook output priority 100; policy accept;
              }

              chain input {
                type filter hook input priority 0; policy drop;

                iifname { "${lan1.iface}", "${lan2.iface}", "${lan3.iface}" } accept
                iifname "${wan}" ct state { established, related } accept
                iifname "${wan}" drop
                iifname "lo" accept
              }

              chain forward {
                type filter hook forward priority filter; policy drop;

                #ip protocol { tcp, udp } flow offload @f

                iifname { "${lan1.iface}", "${lan2.iface}", "${lan3.iface}" } oifname { "${wan}" } accept
                iifname { "${wan}" } oifname { "${lan1.iface}", "${lan2.iface}", "${lan3.iface}" } ct state { established, related } accept
              }
            }

            table ip nat {
              chain prerouting {
                type nat hook prerouting priority -100; policy accept;
              }

              chain postrouting {
                type nat hook postrouting priority 100; policy accept;
                oifname "${wan}" masquerade 
              }
            }
          '';
        };
      };

      environment.systemPackages = with pkgs; [
        htop
        vim
        wget
        dig
        kitty
        ethtool
        tcpdump
        conntrack-tools
      ];

      services = {
        resolved.enable = false;
        openssh = {
          enable = true;
        };

        adguardhome = {
          enable = true;
          settings = {
            http = {
              address = "0.0.0.0:3000";
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
            filters =
              map
                (url: {
                  enabled = true;
                  url = url;
                })
                [
                  "https://adguardteam.github.io/HostlistsRegistry/assets/filter_9.txt"
                  "https://adguardteam.github.io/HostlistsRegistry/assets/filter_11.txt"
                  "https://raw.githubusercontent.com/AdguardTeam/FiltersRegistry/master/filters/filter_17_TrackParam/filter.txt"
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
            };
            forward-zone = [
              {
                name = ".";
                forward-addr = [
                  "1.1.1.1@853#cloudflare-dns.com"
                  "1.0.0.1@853#cloudflare-dns.com"
                ];
                forward-first = false;
                forward-tls-upstream = true;
              }
            ];
          };
        };

        kea.dhcp4 = {
          enable = true;
          settings = {
            interfaces-config = {
              interfaces = [
                lan1.iface
                lan2.iface
                lan3.iface
              ];
            };
            lease-database = {
              name = "/var/lib/kea/dhcp4.leases";
              persist = true;
              type = "memfile";
            };
            rebind-timer = 2000;
            renew-timer = 1000;
            subnet4 = [
              {
                id = 1;
                pools = [
                  {
                    pool = "10.10.51.100 - 10.10.51.240";
                  }
                ];
                subnet = lan1.subnet;
                interface = lan1.iface;
                option-data = [
                  {
                    "name" = "routers";
                    "data" = lan1.gw;
                  }
                  {
                    "name" = "domain-name-servers";
                    "data" = lan1.gw;
                  }
                ];
              }
              {
                id = 2;
                pools = [
                  {
                    pool = "10.10.53.100 - 10.10.53.240";
                  }
                ];
                subnet = lan2.subnet;
                interface = lan2.iface;
                option-data = [
                  {
                    "name" = "routers";
                    "data" = lan2.gw;
                  }
                  {
                    "name" = "domain-name-servers";
                    "data" = lan2.gw;
                  }
                ];
              }
              {
                id = 3;
                pools = [
                  {
                    pool = "10.10.55.100 - 10.10.55.240";
                  }
                ];
                subnet = lan3.subnet;
                interface = lan3.iface;
                option-data = [
                  {
                    "name" = "routers";
                    "data" = lan3.gw;
                  }
                  {
                    "name" = "domain-name-servers";
                    "data" = lan3.gw;
                  }
                ];
              }
            ];
            valid-lifetime = 4000;
          };
        };

        avahi = {
          enable = true;
          reflector = true;
          interfaces = [
            lan1.iface
            lan2.iface
            lan3.iface
          ];
        };
      };

      sops.defaultSopsFile = ../host-secrets/router-secrets.yaml;
      sops.secrets.whitehead-password = { };
      sops.secrets.whitehead-password.neededForUsers = true;
    };
}
