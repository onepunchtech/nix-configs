{
  enable = true;
  overlay.enable = true;
  systemd.enable = true;
  hyprland.enable = true;
  overwrite.enable = true;
  theme = "catppuccin_mocha";
  # override = {
  #   theme.bar.menus.text = "#123ABC";
  # };

  layout = {
    "bar.layouts" = {
      "*" = {
        left = [
          "dashboard"
          "workspaces"
          "windowtitle"
        ];
        middle = [
          "media"
          "clock"
        ];
        right = [
          "volume"
          "network"
          "bluetooth"
          "ram"
          "cpu"
          "cputemp"
          "systray"
          "notifications"
        ];
      };
    };
  };

  settings = {
    bar.launcher.autoDetectIcon = true;
    bar.workspaces = {
      show_numbered = true;
    };

    bar.customModules.cpuTemp.sensor = "/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon1/temp1_input";

    menus.clock = {
      time = {
        military = false;
        hideSeconds = true;
      };
      weather.unit = "metric";
    };

    menus.dashboard.directories.enabled = false;
    menus.dashboard.stats.enable_gpu = true;

    theme.bar.transparent = false;

    theme.font = {
      name = "Nordzy";
      size = "16px";
    };
  };
}
