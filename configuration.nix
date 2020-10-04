{ config, pkgs, ... }:

let
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in
{
  imports =
    [ 
      <nixos-hardware/common/cpu/intel>
      <nixos-hardware/common/pc/laptop>
      <nixos-hardware/common/pc/laptop/ssd>
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  nix.nixPath = [
    (let
      sshConfigFile = pkgs.writeText "ssh_config" ''
            Host github.com
            IdentityFile /etc/ssh/ssh_host_rsa_key
            StrictHostKeyChecking=no
        '';
    in
      "ssh-config-file=${sshConfigFile}"
    )
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs"
    "nixos-config=/etc/nixos/configuration.nix"
    "/nix/var/nix/profiles/per-user/root/channels"
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages_latest;
    kernelPatches = [{
      name = "thunderbolt";
      patch = null;
      extraConfig = ''
        THUNDERBOLT y
        HOTPLUG_PCI y
        HOTPLUG_PCI_ACPI y
      '';
    }];
    # blacklistedKernelModules = ["nouveau"];
    blacklistedKernelModules = [ "nouveau" "nv" "rivafb" "nvidiafb" "rivatv" ];
    tmpOnTmpfs = true;
  };

  console.font = "latarcyrheb-sun32";

  environment.systemPackages = with pkgs; [
    wget 
    vim
    acpitool
    emacs
    git
    tree
    rxvt_unicode
    unstable.home-manager
    lm_sensors
    firmwareLinuxNonfree
  ];

  fonts.fonts = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    font-awesome_5
    material-icons
  ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
  };

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    nvidia = {
      optimus_prime.enable = true;
      modesetting.enable = true;
      optimus_prime = {
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
    };
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };

  networking = {
    firewall.enable = false;
    hostName = "ludwig";
    networkmanager.enable = true;
  };
  
  services = {
    acpid.enable = true;
    hardware.bolt.enable = true;
    openssh.enable = true;
    printing.enable = true;
    tlp.enable = true;
    resolved = {
      enable = true;
      fallbackDns = [ "1.1.1.1" ];
    };
    undervolt = {
      enable = true;
      analogioOffset = "-100";
      coreOffset = "-130";
      uncoreOffset = "-100";
      gpuOffset = "-50"; 
      verbose = true;
    };
    upower.enable = true;
    xserver = {
      enable = true;
      layout = "us";
      xkbModel = "pc104";
      xkbOptions = "caps:super,ctrl:swap_lalt_lctl";
      dpi = 220;
      libinput = {
        naturalScrolling = false;
        enable = true;
        middleEmulation = true;
        tapping = true;
        clickMethod = "clickfinger";
      };
      videoDrivers = [ "nvidia" ];
    };
  };

  sound.enable = true;

  system.stateVersion = "19.03";

  time.timeZone = "America/Chicago";

  users.users.whitehead = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" "docker" ];
  };

  virtualisation.docker.enable = true;
}
