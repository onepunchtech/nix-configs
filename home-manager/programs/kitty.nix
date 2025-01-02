{
  enable = true;
  font.name = "DejaVu Sans Mono";
  font.size = 10;
  settings = {
    scrollback_lines = 10000;
    enable_audio_bell = false;
    update_check_interval = 0;
    tab_bar_style = "hidden";
    confirm_os_window_close = 0;
    tab_title_template = "{title}{' :{}:'.format(num_windows) if num_windows > 1 else ''}";
  };
  themeFile = "Catppuccin-Mocha";
  extraConfig = '''';
}
