#!/bin/bash

SINK=$( pactl list short sinks | grep RUNNING | sed -e 's,^\([0-9][0-9]*\)[^0-9].*,\1,' | head -n 1 )

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
