{
  pkgs,
  config,
  lib,
  ...
}:

{

  imports = [
    ./lib/base.nix
    ./lib/users.nix
    ./lib/shell.nix
    ./lib/sops.nix
  ];

  options = {
    options.services.unbound.settings.server.local-data = lib.mkOption {
      type = lib.types.anything;
    };

    options.services.unbound.settings.server.local-zone = lib.mkOption {
      type = lib.types.anything;
    };

  };
  config = {

    boot.kernel.sysctl = {
      "net.ipv4.conf.all.forwarding" = true;
      "net.ipv6.conf.all.forwarding" = true;
    };

    systemd.network.networks."10-lan" = {
      matchConfig.Name = "lan";
      networkConfig.DHCP = "ipv4";
    };

    # networking = {
    #   useDHCP = false;
    #   firewall = {
    #     enable = false;
    #   };
    #   hostName = "labcontrol";
    #   networkmanager.enable = false;
    #   interfaces.enp1s0.ipv4.addresses = [
    #     {
    #       address = "10.10.10.11";
    #       prefixLength = 24;
    #     }
    #   ];
    #
    #   defaultGateway = {
    #     address = "10.10.10.1";
    #     interface = "enp1s0";
    #   };
    #   nameservers = [ "127.0.0.1" ];
    # };

    environment.systemPackages = with pkgs; [
      htop
      vim
      wget
      dig
      kitty
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
    };

    sops.defaultSopsFile = ./host-secrets/labcontrol-secrets.yaml;
    sops.secrets.whitehead-password = { };
    sops.secrets.whitehead-password.neededForUsers = true;
  };
}
