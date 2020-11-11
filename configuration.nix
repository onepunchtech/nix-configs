{ config, pkgs, ... }:

let
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in
{
  imports =
    [ 
      <nixos-hardware/dell/xps/15-7590>
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

  nix = {
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    binaryCaches = [ "https://hydra.iohk.io" ];
    buildMachines = [ {
      hostName = "builder";
      system = "x86_64-linux";
      maxJobs = 1;
      speedFactor = 2;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      mandatoryFeatures = [ ];
    }] ;
    distributedBuilds = true;
    gc = {
      automatic = true;
      dates = "weekly";
    };
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages_latest;
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
    nvidia-offload
  ];

  fonts.fonts = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    font-awesome_5
    material-icons
  ];

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    nvidia = {
      prime = {
        offload.enable = true;
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
    extraHosts = ''
      192.168.1.12 bigtux
      192.168.1.12 builder
    '';
    networkmanager.enable = true;
  };
  
  services = {
    acpid.enable = true;
    hardware.bolt.enable = true;
    openssh.enable = true;
    printing.enable = true;
    printing.drivers = [pkgs.brlaser];
    tlp.enable = true;
    resolved = {
      enable = true;
      fallbackDns = [ "1.1.1.1" ];
    };
    undervolt = {
      enable = true;
      analogioOffset = -100;
      coreOffset = -130;
      uncoreOffset = -100;
      gpuOffset = -50; 
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
      videoDrivers = [ "modesetting" "nvidia" ];
    };
  };

  sound.enable = true;

  system.stateVersion = "19.03";

  time.timeZone = "America/Denver";

  users.users.whitehead = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" "docker" ];
  };

  virtualisation.docker.enable = true;
}
