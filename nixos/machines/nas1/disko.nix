{
  disko.devices = {
    disk = {
      os = {
        type = "disk";
        device = "/dev/disk/by-id/ata-PNY_250GB_SATA_SSD_PND14252388180200182";
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
      # data1 = {
      #   type = "disk";
      #   device = "/dev/disk/by-id/ata-ST4000VN008-2DR166_ZGY9B5CG";
      #   content = {
      #     type = "gpt";
      #     partitions = {
      #       zfs = {
      #         size = "100%";
      #         content = {
      #           type = "zfs";
      #           pool = "storage1";
      #         };
      #       };
      #     };
      #   };
      # };
      #
      # data2 = {
      #   type = "disk";
      #   device = "/dev/disk/by-id/ata-ST4000VN008-2DR166_ZGY9B579";
      #   content = {
      #     type = "gpt";
      #     partitions = {
      #       zfs = {
      #         size = "100%";
      #         content = {
      #           type = "zfs";
      #           pool = "storage1";
      #         };
      #       };
      #     };
      #   };
      # };
      #
      # data3 = {
      #   type = "disk";
      #   device = "/dev/disk/by-id/ata-ST4000VN008-2DR166_ZGY8ZJBZ";
      #   content = {
      #     type = "gpt";
      #     partitions = {
      #       zfs = {
      #         size = "100%";
      #         content = {
      #           type = "zfs";
      #           pool = "storage1";
      #         };
      #       };
      #     };
      #   };
      # };
      #
      # data4 = {
      #   type = "disk";
      #   device = "/dev/disk/by-id/ata-ST4000VN008-2DR166_ZGY8ZHFB";
      #   content = {
      #     type = "gpt";
      #     partitions = {
      #       zfs = {
      #         size = "100%";
      #         content = {
      #           type = "zfs";
      #           pool = "storage1";
      #         };
      #       };
      #     };
      #   };
      # };
      #
      # data5 = {
      #   type = "disk";
      #   device = "/dev/disk/by-id/ata-ST4000VN008-2DR166_ZGY8YVDE";
      #   content = {
      #     type = "gpt";
      #     partitions = {
      #       zfs = {
      #         size = "100%";
      #         content = {
      #           type = "zfs";
      #           pool = "storage1";
      #         };
      #       };
      #     };
      #   };
      # };
      #
      # data6 = {
      #   type = "disk";
      #   device = "/dev/disk/by-id/ata-ST4000VN008-2DR166_ZGY8ZF8S";
      #   content = {
      #     type = "gpt";
      #     partitions = {
      #       zfs = {
      #         size = "100%";
      #         content = {
      #           type = "zfs";
      #           pool = "storage1";
      #         };
      #       };
      #     };
      #   };
      # };
      # log1 = {
      #   type = "disk";
      #   device = "/dev/disk/by-id/ata-PNY_250GB_SATA_SSD_PND14252388180200184";
      #   content = {
      #     type = "gpt";
      #     partitions = {
      #       zfs = {
      #         size = "16G";
      #         content = {
      #           type = "zfs";
      #           pool = "storage1";
      #         };
      #       };
      #     };
      #   };
      # };
      # log2 = {
      #   type = "disk";
      #   device = "/dev/disk/by-id/nvme-Samsung_SSD_960_EVO_250GB_S3ESNX0J602156V";
      #   content = {
      #     type = "gpt";
      #     partitions = {
      #       zfs = {
      #         size = "16G";
      #         content = {
      #           type = "zfs";
      #           pool = "storage1";
      #         };
      #       };
      #     };
      #   };
      # };
    };
    # zpool = {
    #   storage1 = {
    #     type = "zpool";
    #     mode = {
    #       topology = {
    #         type = "topology";
    #         vdev = [
    #           {
    #             mode = "raidz2";
    #             members = [
    #               "data1"
    #               "data2"
    #               "data3"
    #               "data4"
    #               "data5"
    #               "data6"
    #             ];
    #           }
    #
    #         ];
    #         log = [
    #           {
    #             mode = "mirror";
    #             members = [
    #               "log1"
    #               "log2"
    #             ];
    #           }
    #         ];
    #       };
    #     };
    #
    #     rootFsOptions = {
    #       compression = "zstd";
    #       atime = "off";
    #       xattr = "sa";
    #       acltype = "posixacl";
    #       mountpoint = "none";
    #       "com.sun:auto-snapshot" = "false";
    #     };
    #     mountpoint = "/mnt/storage1";
    #     datasets = {
    #       zfs_fs = {
    #         type = "zfs_fs";
    #         #mountpoint = "/mnt/storage1/zfs_fs";
    #         options = {
    #           atime = "off";
    #           mountpoint = "/mnt/nas";
    #           "com.sun:auto-snapshot" = "auto";
    #         };
    #       };
    #     };
    #   };
    # };
  };
}
