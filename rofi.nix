let
  bgColor = "#191919";
  textColor = "#555555";
  selectedColor = "#ffffff";
in
{
  enable = true;
  font = "DejaVu Sans Mono 30";
  lines = 5;
  padding = 40;
  separator = "none";
  scrollbar = false;
  terminal = "alacritty";
  width = 30;
  location = "top";
  yoffset = 100;
  rowHeight = 1;

  colors = {
    window = {
      background = "${bgColor}";
      border = "${bgColor}";
      separator = "#c3c6c8";
    };

    rows = {
      normal = {
        background = "${bgColor}";
        foreground = "${textColor}";
        backgroundAlt = "${bgColor}";
        highlight = {
          background = "${bgColor}";
          foreground = "${selectedColor}";
        };
      };

      active = {
        background = "${bgColor}";
        foreground = "${textColor}";
        backgroundAlt = "${bgColor}";
        highlight = {
          background = "${bgColor}";
          foreground = "${selectedColor}";
        };
      };

      urgent = {
        background = "${bgColor}";
        foreground = "${textColor}";
        backgroundAlt = "${bgColor}";
        highlight = {
          background = "${bgColor}";
          foreground = "${selectedColor}";
        };
      };
    };
  };
}
