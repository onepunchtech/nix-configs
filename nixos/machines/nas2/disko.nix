{
  disko.devices = {
    disk = {
      os = {
        type = "disk";
        device = "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_250GB_S5B4NMFN922018B";
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
        device = "/dev/disk/by-id/ata-ST14000VN0008-2JG101_ZHZ6BX33";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "storage1";
              };
            };
          };
        };
      };
      data2 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-ST14000VN0008-2JG101_ZHZ6BX2C";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "storage1";
              };
            };
          };
        };
      };
      data3 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-ST14000VN0008-2JG101_ZTM0NA0M";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "storage1";
              };
            };
          };
        };
      };
      data4 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-ST14000VN0008-2JG101_ZTM0NA0L";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "storage1";
              };
            };
          };
        };
      };
      log1 = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_500GB_S58SNM0T804264F";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "16G";
              content = {
                type = "zfs";
                pool = "storage1";
              };
            };
          };
        };
      };
      log2 = {
        type = "disk";
        device = "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_250GB_S59WNE0M908117R";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "16G";
              content = {
                type = "zfs";
                pool = "storage1";
              };
            };
          };
        };
      };
    };
    zpool = {
      storage1 = {
        type = "zpool";
        mode = {
          topology = {
            type = "topology";
            vdev = [
              {
                mode = "mirror";
                members = [
                  "data1"
                  "data2"
                ];
              }
              {
                mode = "mirror";
                members = [
                  "data3"
                  "data4"
                ];
              }

            ];
            log = [
              {
                mode = "mirror";
                members = [
                  "log1"
                  "log2"
                ];
              }
            ];
          };
        };

        rootFsOptions = {
          compression = "zstd";
          atime = "off";
          xattr = "sa";
          acltype = "posixacl";
          mountpoint = "none";
          "com.sun:auto-snapshot" = "false";
        };
        datasets = {
          zfs_fs = {
            type = "zfs_fs";
            #mountpoint = "/mnt/nas";
            options = {
              atime = "off";
              mountpoint = "/mnt/nas";
              "com.sun:auto-snapshot" = "auto";
            };
          };
        };
      };
    };
  };
}
