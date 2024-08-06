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
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC5vw9x9O3Nz/hayQwjV4w/gwfT7V0pnN5fxNpZuPZGtcFbSZjwNhEQ66RrGlOc9EZZZf6pLckffGBWUNvV24ocrNor0IBvQZhoR/7d/WE+vCxQtQwJW/ZZagiE7noqpRyk5Ty7DwNDi05s8IMvCUQ6i+t+iVLo7TEZelvF3+1NB5Azx+MIbdtr7kmYlZ8SQwJsSIYYHTVgC8+bEQ5bkRjtjsukHoqiuNzA3z3mBqw7jFCq2PbgYbllzDlJSYkfvr2yxCRqeedab1Q/q4kSD1wq9ysqcPEUYJhDVAVnr1Rp/ndVM+N1tMw4R/ilqsghwNbPNIr7yEY7Y37WphOanqM/ whitehead@ludwig"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDXNsJVI+XFBSSfunirxgUFStUCXhFMNBIj2b4Xsl7WSm40E43ZcWThisc+CDGrfstO1w8B2rZSJkBq1xl5IRoyvePy3P/Rcg6tKQIqqBfaksTXZ9ENHP5DwNuYOh1VQCjMBlj5ZWv7n9wXe+fBAS0SXu51ZJX8hiiy5Te9oKOyDm0o3qRXDy/RwWmEzY//g8d+cTcqsRhWaPbpYhEide9Sjbxn4et68/UG5qJgxRNYOWFtCPIeX55sAerfivOKhsx3a0y+C1x1Yhdaf23TvyNKr6jmv4IHwuuZ7uuvicBoKHS9I40+1HOWpDgpgJs8+IwEdDeTJqBlIYMat1xRVA2SP8TruH1aQccSIuYSMgv9XvjS3I6rWe0jjbTTiaWEehc20J01TVzXIdxh1/PgcAJMnvkOu1bDWw9DnHTIhEYSnb3/dKeOj9zQZlwSgZDu42gwJW1IcO133GxJp+EI7y8bQ80Y4WuR6BwEVlM3gFJkphvDqdjDoH9v1Zr+b66MnLc= whitehead@ludwig"
    ];
  };

  users.users.root = {
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC5vw9x9O3Nz/hayQwjV4w/gwfT7V0pnN5fxNpZuPZGtcFbSZjwNhEQ66RrGlOc9EZZZf6pLckffGBWUNvV24ocrNor0IBvQZhoR/7d/WE+vCxQtQwJW/ZZagiE7noqpRyk5Ty7DwNDi05s8IMvCUQ6i+t+iVLo7TEZelvF3+1NB5Azx+MIbdtr7kmYlZ8SQwJsSIYYHTVgC8+bEQ5bkRjtjsukHoqiuNzA3z3mBqw7jFCq2PbgYbllzDlJSYkfvr2yxCRqeedab1Q/q4kSD1wq9ysqcPEUYJhDVAVnr1Rp/ndVM+N1tMw4R/ilqsghwNbPNIr7yEY7Y37WphOanqM/ whitehead@ludwig"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDXNsJVI+XFBSSfunirxgUFStUCXhFMNBIj2b4Xsl7WSm40E43ZcWThisc+CDGrfstO1w8B2rZSJkBq1xl5IRoyvePy3P/Rcg6tKQIqqBfaksTXZ9ENHP5DwNuYOh1VQCjMBlj5ZWv7n9wXe+fBAS0SXu51ZJX8hiiy5Te9oKOyDm0o3qRXDy/RwWmEzY//g8d+cTcqsRhWaPbpYhEide9Sjbxn4et68/UG5qJgxRNYOWFtCPIeX55sAerfivOKhsx3a0y+C1x1Yhdaf23TvyNKr6jmv4IHwuuZ7uuvicBoKHS9I40+1HOWpDgpgJs8+IwEdDeTJqBlIYMat1xRVA2SP8TruH1aQccSIuYSMgv9XvjS3I6rWe0jjbTTiaWEehc20J01TVzXIdxh1/PgcAJMnvkOu1bDWw9DnHTIhEYSnb3/dKeOj9zQZlwSgZDu42gwJW1IcO133GxJp+EI7y8bQ80Y4WuR6BwEVlM3gFJkphvDqdjDoH9v1Zr+b66MnLc= whitehead@ludwig"
    ];
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
  ];


  services = {
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
