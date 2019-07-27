{
  enable = true;
  initExtra = ''
    export PATH=~/.local/bin:$PATH
  '';
  historyControl = [ "erasedups" "ignorespace" ];
  historyFileSize = 1000000;
  historyIgnore = [ "ls" "ps" "history" ];
  shellOptions = [ "histappend" ];
}
