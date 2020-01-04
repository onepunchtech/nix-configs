{
  enable = true;
  initExtra = ''
    export PATH=~/.local/bin:$PATH
  '';
  historyControl = [ "erasedups" "ignorespace" ];
  historySize = 1000000;
  historyIgnore = [ "ls" "ps" "history" ];
  shellOptions = [ "histappend" ];
}
