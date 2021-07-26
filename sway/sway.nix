{alacritty, wofi, waybar, ...}:

let mod = "Mod4";

in {

  enable = true;
  package = null;
  config = {
    modifier = mod;
    window = {
      hideEdgeBorders = "both";
      titlebar = false;
    };
    terminal = "alacritty";
    keybindings = {
      "${mod}+Return" = "exec ${alacritty}/bin/alacritty";
      "${mod}+c" = "kill";
      "${mod}+p" = "exec \"${wofi}/bin/wofi --show drun -i | xargs swaymsg exec --\"";

      "${mod}+h" = "focus left";
      "${mod}+j" = "focus down";
      "${mod}+k" = "focus up";
      "${mod}+l" = "focus right";

      "${mod}+Shift+h" = "move left";
      "${mod}+Shift+j" = "move down";
      "${mod}+Shift+k" = "move up";
      "${mod}+Shift+l" = "move right";

      "${mod}+b" = "split h";
      "${mod}+v" = "split v";
      "${mod}+f" = "fullscreen toggle";

      "${mod}+s" = "layout stacking";
      "${mod}+w" = "layout tabbed";
      "${mod}+e" = "layout toggle split";

      "${mod}+Shift+space" = "floating toggle";

      "${mod}+space" = "focus mode_toggle";
      "${mod}+a" = "focus parent";

      "${mod}+1" = "workspace 1";
      "${mod}+2" = "workspace 2";
      "${mod}+3" = "workspace 3";
      "${mod}+4" = "workspace 4";
      "${mod}+5" = "workspace 5";
      "${mod}+6" = "workspace 6";
      "${mod}+7" = "workspace 7";
      "${mod}+8" = "workspace 8";
      "${mod}+9" = "workspace 9";
      "${mod}+0" = "workspace 10";

      "${mod}+Shift+1" = "move container to workspace 1";
      "${mod}+Shift+2" = "move container to workspace 2";
      "${mod}+Shift+3" = "move container to workspace 3";
      "${mod}+Shift+4" = "move container to workspace 4";
      "${mod}+Shift+5" = "move container to workspace 5";
      "${mod}+Shift+6" = "move container to workspace 6";
      "${mod}+Shift+7" = "move container to workspace 7";
      "${mod}+Shift+8" = "move container to workspace 8";
      "${mod}+Shift+9" = "move container to workspace 9";
      "${mod}+Shift+0" = "move container to workspace 10";

      "${mod}+Shift+c" = "reload";
      "${mod}+Shift+r" = "restart";
      "${mod}+Shift+e" = "exit";

      "${mod}+r" = "mode \"resize\"";

      "XF86AudioMute" = "exec opdt -m";
      "XF86AudioLowerVolume" = "exec opdt -d 5%";
      "XF86AudioRaiseVolume" = "exec opdt -i 5%";

      "XF86MonBrightnessUp" = "exec brightnessctl set +5%";
      "XF86MonBrightnessDown" = "exec brightnessctl set 5%-";
    };

    input = {
      "type:keyboard" = {
        "xkb_layout" = "us";
        "xkb_options" = "caps:super,ctrl:swap_lalt_lctl";
      };
    };

    bars = [
      {command = "${waybar}/bin/waybar";}
    ];
  };
}
