let themes = {
      dark = {
        primary = {
          background = "0x002b36";
          foreground = "0x839496";
        };

        normal = {
          black =   "0x073642";
          red =     "0xdc322f";
          green =   "0x859900";
          yellow =  "0xb58900";
          blue =    "0x268bd2";
          magenta = "0xd33682";
          cyan =    "0x2aa198";
          white =   "0xeee8d5";
        };

        bright = {
          black =   "0x002b36";
          red =     "0xcb4b16";
          green =   "0x586e75";
          yellow =  "0x657b83";
          blue =    "0x839496";
          magenta = "0x6c71c4";
          cyan =    "0x93a1a1";
          white =   "0xfdf6e3";
        };
      };

      light = {
        primary = {
          background = "#fdf6e3";
          foreground = "#657b83";
        };
        cursor = {
          text =  "#fdf6e3";
          cursor = "#657b83";
        };
        normal = {
          black =  "#073642";
          red =    "#dc322f";
          green =  "#859900";
          yellow = "#b58900";
          blue =   "#268bd2";
          magenta ="#d33682";
          cyan =   "#2aa198";
          white =  "#eee8d5";
        };

        # Bright colors
        bright = {
          black =  "#002b36";
          red =    "#cb4b16";
          green =  "#586e75";
          yellow = "#657b83";
          blue =   "#839496";
          magenta ="#6c71c4";
          cyan =   "#93a1a1";
          white =  "#fdf6e3";
        };
      };
    };

in {
  enable = true;
  settings = {
    env.TERM = "xterm-256color";
    window.dimensions = {
      lines = 3;
      columns = 200;
    };
    font = {
      size = 8.0;
    };
    colors = themes.dark;
  };
}
