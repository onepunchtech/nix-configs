{pkgs, ...}:

{
  emc = pkgs.writeShellScriptBin "emc" ''
    tty -s;
    if [ "0" == "$?" ]; then
        emacsclient -a "" -t "$@"
    else
        emacsclient -a "" -n -c "$@"
    fi
  '';
  pavol = pkgs.writeShellScriptBin "pavol.sh" ''
    SINK=$( pactl list short sinks | grep -E 'SUSPENDED|RUNNING' | sed -e 's,^\([0-9][0-9]*\)[^0-9].*,\1,' | head -n 1 )

    if [ "$1" == "up" ]
    then
            pactl set-sink-mute $SINK 0
            pactl set-sink-volume $SINK +5%

    elif [ "$1" == "down" ]
    then
            pactl set-sink-mute $SINK 0
            pactl set-sink-volume $SINK -5%

    elif [ "$1" == "toggle" ]
    then
            pactl set-sink-mute $SINK toggle
    fi
  '';
}