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
      wan = "enp4s0";

      lan1 = {
        iface = "eno1";
        gw = "10.10.51.1";
        ip = "10.10.51.1/24";
        subnet = "10.10.51.0/24";
      };

      lan2 = {
        iface = "eno2";
        gw = "10.10.53.1";
        ip = "10.10.53.1/24";
        subnet = "10.10.53.0/24";
      };

      lan3 = {
        iface = "enp5s0";
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

      boot.kernelModules = [
        "nf_flow_table_inet"
        "nf_flow_table"
      ];

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
          checkRuleset = false;
          ruleset = ''
            table inet filter {

              #flowtable f {
              #  hook ingress priority 0;
              #  devices = { ${wan}, ${lan1.iface}, ${lan2.iface}, ${lan3.iface} };
              #}

              chain output {
                type filter hook output priority 100; policy accept;
              }

              chain input {
                type filter hook input priority 0; policy drop;

                iifname "lo" accept
                iifname { "${lan1.iface}", "${lan2.iface}", "${lan3.iface}" } accept
                iifname "${wan}" ct state { established, related } accept
                iifname "${wan}" drop
              }

              chain forward {
                type filter hook forward priority filter; policy drop;

                #ip protocol { tcp, udp } flow offload @f

                iifname { "${lan1.iface}", "${lan2.iface}", "${lan3.iface}" } oifname { "${wan}" } accept
                iifname { "${lan1.iface}", "${lan2.iface}", "${lan3.iface}" } oifname { "${lan1.iface}", "${lan2.iface}", "${lan3.iface}" } accept
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
              local_ptr_upstreams = [
                "[/in-addr.arpa/]127.0.0.1:5335"
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
        unbound =
          let
            onepunchZone = pkgs.writeText "onepunch.zone" ''
              $ORIGIN onepunch.
              $TTL 86400
              @       IN      SOA     ns1.onepunch. admin.onepunch. (
                                      2023010101 ; serial
                                      3600       ; refresh
                                      1800       ; retry
                                      1209600    ; expire
                                      86400 )    ; minimum
                               IN      NS      ns1.onepunch.
              ns1              IN      A       10.10.53.1
              router           IN      A       10.10.53.1
              tplink1          IN      A       10.10.53.5
              bigtux           IN      A       10.10.51.140
              officelab        IN      A       10.10.51.41

              k8scontrol1      IN      A       10.10.51.41
              cp1.officelab    IN      A       10.10.51.41
              k8scontrol2      IN      A       10.10.51.42
              cp2.officelab    IN      A       10.10.51.42
              k8scontrol3      IN      A       10.10.51.43
              cp3.officelab    IN      A       10.10.51.43

              masterlab        IN      A       10.10.51.31
              cp1.masterlab    IN      A       10.10.51.31
              nas.masterlab    IN      A       10.10.51.39


            '';

            reverseOnepunchZone = pkgs.writeText "" ''
              $ORIGIN 51.10.10.in-addr.arpa.
              $TTL 86400
              @       IN      SOA     ns1.onepunch. admin.onepunch. (
                                      2023010101 ; serial
                                      3600       ; refresh
                                      1800       ; retry
                                      1209600    ; expire
                                      86400 )    ; minimum
                  IN NS ns1.onepunch.

              1   IN PTR router.onepnuch.
              41 IN PTR officelab.onepnuch.
              41 IN PTR k8scontrol1.onepnuch.
              41 IN PTR cp1.officelab.onepnuch.
              42 IN PTR k8scontrol2.onepnuch.
              42 IN PTR cp2.officelab.onepnuch.
              43 IN PTR k8scontrol3.onepnuch.
              43 IN PTR cp3.officelab.onepnuch.

              31 IN PTR cp1.masterlab
              31 IN PTR masterlab
              39 IN PTR nas.masterlab
            '';

            reverse53OnepunchZone = pkgs.writeText "" ''
              $ORIGIN 53.10.10.in-addr.arpa.
              $TTL 86400
              @       IN      SOA     ns1.onepunch. admin.onepunch. (
                                      2023010101 ; serial
                                      3600       ; refresh
                                      1800       ; retry
                                      1209600    ; expire
                                      86400 )    ; minimum
                  IN NS ns1.onepunch.

              1   IN PTR router.onepnuch.
              5  IN PTR tplink1.onepnuch.
            '';

          in
          {
            enable = true;
            settings = {
              server = {
                #verbosity = 2;
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
                private-domain = [
                  "onepunch."
                  "51.10.10.in-addr.arpa."
                ];
                local-zone = [
                  "\"10.in-addr.arpa.\" nodefault"
                ];
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
              auth-zone = [
                {
                  name = "onepunch";
                  zonefile = "${onepunchZone}";
                }
                {
                  name = "51.10.10.in-addr.arpa";
                  zonefile = "${reverseOnepunchZone}";
                }
                {
                  name = "53.10.10.in-addr.arpa";
                  zonefile = "${reverse53OnepunchZone}";
                }
              ];
            };
          };

        kea.dhcp4 = {
          enable = true;
          settings = {
            interfaces-config = {
              interfaces = [
                "${lan1.iface}/${lan1.gw}"
                "${lan2.iface}/${lan2.gw}"
                "${lan3.iface}/${lan3.gw}"
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
                    pool = "10.10.51.140 - 10.10.51.240";
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
                reservations = [

                  {
                    hw-address = "00:e0:4c:68:07:9f";
                    ip-address = "10.10.51.41"; # k8scontrol1
                  }
                  {
                    hw-address = "34:1a:4d:0e:9b:ee";
                    ip-address = "10.10.51.42"; # k8scontrol2
                  }
                  {
                    hw-address = "34:1a:4d:0e:9f:49";
                    ip-address = "10.10.51.43"; # k8scontrol3
                  }
                  {
                    hw-address = "6c:bf:b5:02:3d:a6";
                    ip-address = "10.10.51.39"; # nas
                  }
                  {
                    hw-address = "7c:10:c9:26:6d:9b";
                    ip-address = "10.10.51.31"; # cp1.masterlab
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

                reservations = [
                  {
                    hw-address = "00:5f:67:72:18:da";
                    ip-address = "10.10.53.5"; # tplink1
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

        frr = {
          bgpd.enable = true;
          config = ''
            ! -*- bgp -*-
            !
            hostname $UDMP_HOSTNAME
            password zebra
            frr defaults traditional
            log file stdout
            !
            router bgp 65000
             bgp ebgp-requires-policy
             bgp router-id 10.10.51.1
             !
             neighbor kubevip peer-group
             neighbor kubevip remote-as 65000
             neighbor kubevip activate
             neighbor kubevip soft-reconfiguration inbound
             neighbor 10.10.51.41 peer-group kubevip
             neighbor 10.10.51.42 peer-group kubevip
             neighbor 10.10.51.43 peer-group kubevip

             address-family ipv4 unicast
              redistribute connected
              neighbor kubevip activate
              neighbor kubevip route-map ALLOW-ALL in
              neighbor kubevip route-map ALLOW-ALL out
              neighbor kubevip next-hop-self
             exit-address-family
             !
            route-map ALLOW-ALL permit 10
            !
            line vty
            !
          '';
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
