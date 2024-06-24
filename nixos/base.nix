{pkgs, ...}:

{
  nixpkgs.config = {
    allowUnfree = true;
  };


  nixpkgs.overlays = [
    (self: super: {
      wl-clipboard-x11 = super.stdenv.mkDerivation rec {
        pname = "wl-clipboard-x11";
        version = "5";

        src = super.fetchFromGitHub {
          owner = "brunelli";
          repo = "wl-clipboard-x11";
          rev = "v${version}";
          sha256 = "1y7jv7rps0sdzmm859wn2l8q4pg2x35smcrm7mbfxn5vrga0bslb";
        };

        dontBuild = true;
        dontConfigure = true;
        propagatedBuildInputs = [ super.wl-clipboard ];
        makeFlags = [ "PREFIX=$(out)" ];
        };
      xsel = self.wl-clipboard-x11;
      xclip = self.wl-clipboard-x11;
    })
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      systemd-boot.configurationLimit = 10;
      efi.canTouchEfiVariables = true;
    };
    supportedFilesystems = [ "ntfs" ];
  };

  nix = {
    buildMachines = [
      {
        hostName = "builder";
        system = "x86_64-linux";
        maxJobs = 1;
        speedFactor = 2;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      }
    ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 1w";
    };

    settings = {
      trusted-public-keys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      substituters = [
        "https://cache.iog.io"
      ];
      trusted-users = [ "@wheel" ];
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };
  };

  networking = {
    useDHCP = false;
    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
    };
    firewall.enable = false;
    extraHosts = ''
      10.10.10.5 bigtux
      10.10.10.5 builder
    '';
  };

  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    tree
    rxvt_unicode
    home-manager
    brightnessctl
    firmwareLinuxNonfree
    gtk-engine-murrine
    gtk_engines
    gsettings-desktop-schemas
    lxappearance
    rofi
  ];

  fonts.packages = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    font-awesome_5
  ];

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
  };

  security.rtkit.enable = true;

  services = {
    openssh.enable = true;
    printing.enable = true;
    printing.drivers = with pkgs; [brlaser gutenprint gutenprintBin cnijfilter2 canon-cups-ufr2 carps-cups];
    avahi.enable = true;
    avahi.nssmdns4 = true;
    acpid.enable = true;
    resolved.enable = true;
    keyd = {
      enable = true;
      keyboards.default.settings = {
        main = {
          capslock = "layer(meta)";
          leftalt = "layer(control)";
          leftcontrol = "layer(alt)";
	};
      };
    };
    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
    };
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    logind = {
      lidSwitch = "ignore";
      lidSwitchDocked = "ignore";
    };
  };

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  systemd.targets.sleep.enable = false;
  systemd.targets.hibernate.enable = false;
  systemd.targets.suspend.enable = false;
  systemd.targets.hybrid-sleep.enable = false;

  hardware.pulseaudio.enable = false;

  time.timeZone = "US/Central";

  users.users.whitehead = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" "docker"];
  };

  virtualisation.docker.enable = true;

  fileSystems."/mnt/share" = {
      device = "//10.10.10.5/public";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

      in ["${automount_opts}"];
  };
  system.stateVersion = "24.05";
}
