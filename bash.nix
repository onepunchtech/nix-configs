{
  enable = true;
  initExtra = ''
    export GOPATH=~/go
    export PATH=~/.local/bin:$GOPATH/bin:$PATH
    export MOZ_ENABLE_WAYLAND=1
    if [[ -z "$TMUX" ]]; then
      tmux has-session -t main || tmux new-session -d -s main
      tmux new-session -t main
    fi
  '';
  historyControl = [ "erasedups" "ignorespace" ];
  historySize = 1000000;
  historyIgnore = [ "ls" "ps" "history" ];
  shellOptions = [ "histappend" ];
}
