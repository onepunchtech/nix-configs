{pkgs, ...}:

{

  fileSystems."/mnt/share" = {
      device = "//10.10.10.5/public";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

      in ["${automount_opts}"];
  };

  buildMachines = [
    {
      hostName = "builder";
      system = "x86_64-linux";
      maxJobs = 1;
      speedFactor = 2;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    }
  ];

  extraHosts = ''
    10.10.10.5 bigtux
    10.10.10.5 builder
  '';

  hardware.bluetooth.enable = true;
}
