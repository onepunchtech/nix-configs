{ config, pkgs, ... }:

let
  unstable = import
    (builtins.fetchTarball https://github.com/nixos/nixpkgs/tarball/master)
    { config = config.nixpkgs.config; };

in
{
  imports =
    [ 
      ./hardware-configuration.nix
    ];
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
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = unstable.linuxPackages_latest;
    tmpOnTmpfs = true;
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
    };
    trustedUsers = [ "@wheel" ];
  };

  networking = {
    useDHCP = false;
    hostName = "ludwig";
    networkmanager.enable = true;
    extraHosts = ''
      10.10.10.5 bigtux
      10.10.10.5 builder
    '';
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
  };

  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    tree
    rxvt_unicode
    unstable.home-manager
    brightnessctl
    firmwareLinuxNonfree
    gtk-engine-murrine
    gtk_engines
    gsettings-desktop-schemas
    lxappearance
  ];

  fonts.fonts = with pkgs; [
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
    printing.drivers = [pkgs.brlaser];
    acpid.enable = true;
    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      layout = "us";
      xkbModel = "pc104";
      xkbOptions = "caps:super,ctrl:swap_lalt_lctl";
      videoDrivers = [ "amdgpu" ];
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

  programs.nm-applet = {
    enable = true;
  };
  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [
      swaylock
      swayidle
      xwayland
      waybar
      mako
      kanshi
    ];
  };

  systemd.targets.sleep.enable = false;
  systemd.targets.hibernate.enable = false;
  systemd.targets.suspend.enable = false;
  systemd.targets.hybrid-sleep.enable = false;

  hardware.pulseaudio.enable = false;

  system.stateVersion = "21.05";

  time.timeZone = "America/Denver";
 
  users.users.whitehead = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" ];
  };
}

