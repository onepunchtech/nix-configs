{
  disko.devices = {
    disk = {
      os = {
        type = "disk";
        device = "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_250GB_S59WNE0M908117R";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "500M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
      data1 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-ST14000VN0008-2JG101_ZTM0NA0M";
        content = {
          type = "gpt";
          partitions = {
            root = {
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = [
                  "-f"
                  "-m raid1"
                  "-d raid1"
                  "/dev/disk/by-id/ata-ST14000VN0008-2JG101_ZTM0NA0L"
                  "/dev/disk/by-id/ata-ST4000VN008-2DR166_ZGY9AQK7"
                  "/dev/disk/by-id/ata-ST4000VN008-2DR166_ZGY9B4JZ"
                ];
                mountpoint = "/mnt/data1";
                mountOptions = [
                  "rw"
                  "ssd_spread"
                  "max_inline=256"
                  "commit=150"
                  "compress=zstd"
                  "noatime"
                  "discard=async"
                ];
              };
            };
          };
        };
      };
    };
  };
}
