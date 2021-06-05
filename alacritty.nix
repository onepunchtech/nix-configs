let
  gruvboxDark = "#282828";
  themes = {
      dark = {
        primary = {
          background = "#282828";
          foreground = "#fbf1c7";
          bright_foreground = "#f9f5d7";
          dim_foreground = "#f2e5bc";
        };

        normal = {
          black =   gruvboxDark;
          red =     "#cc241d";
          green =   "#98971a";
          yellow =  "#d79921";
          blue =    "#458588";
          magenta = "#b16286";
          cyan =    "#689d6a";
          white =   "#a89984";
        };

        bright = {
          black =   "#928374";
          red =     "#fb4934";
          green =   "#b8bb26";
          yellow =  "#fabd2f";
          blue =    "#83a598";
          magenta = "#d3869b";
          cyan =    "#8ec07c";
          white =   "#ebdbb2";
        };

        dim = {
          black =   "#32302f";
          red =     "#9d0006";
          green =   "#79740e";
          yellow =  "#b57614";
          blue =    "#076678";
          magenta = "#8f3f71";
          cyan =    "#427b58";
          white =   "#928374";
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
