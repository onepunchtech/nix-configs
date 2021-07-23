{
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
    import = ["~/.config/opdt/alacritty-theme.yml"];
  };
}
