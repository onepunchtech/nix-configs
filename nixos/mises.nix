{pkgs, config, ...}:
{
  imports = [
    ./base.nix
    ./base-hardware.nix
  ];

  networking.hostName = "mises";

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = [
      pkgs.amdvlk
    ];
  };

  fileSystems."/storage1" =
    { device = "/dev/disk/by-label/Storage1";
      fsType = "ext4";
      options = [
        "users"
        "nofail"
      ];
    };

  fileSystems."/extra1" =
    { device = "/dev/disk/by-label/Extra1";
      fsType = "ext4";
      options = [
        "users"
        "nofail"
      ];
    };

  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
  services.xserver.videoDrivers = [ "amdgpu" ];
  boot.kernelParams = [
    "video=DP-1:2560x1440@120"
    "video=HDMI-A-1:2560x1440@120"
  ];
}
