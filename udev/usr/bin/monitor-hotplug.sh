#!/bin/bash

#Adapt this script to your needs.

DEVICES=$(find /sys/class/drm/*/status)

#this while loop declare the $DP1, $DP2, $HDMI2 and others if they are plugged in
while read l; do
	dir=$(dirname $l);
	status=$(cat $l);
	dev=$(echo $dir | cut -d\- -f 2-);

	if [ $(expr match $dev "HDMI") != "0" ] && [ "connected" == "$status" ]; then
		echo $dev "connected"
		dev=HDMI${dev#HDMI-?-}
		declare $dev="yes"
		break
	elif [ $(expr match $dev "DP") != "0" ] && [ "connected" == "$status" ]; then
		echo $dev "connected"
		dev=$(echo $dev | tr -d '-')
		declare $dev="yes"
		break
	fi
done <<< "$DEVICES"


if [ ! -z "$HDMI2" ]; then
	xrandr --output DP1 --auto --off --output DP2 --auto --off --output HDMI2 --auto --pos 0x0 --scale 1.75x1.75 --output eDP1 --auto --pos 0x1890 --primary
elif [ ! -z "$DP1" ]; then
	xrandr --output DP2 --auto --off --output HDMI2 --auto --off --output DP1 --auto --pos 0x0 --scale 1.5x1.5 --output eDP1 --auto --pos 3840x0 --primary
elif [ ! -z "$DP2" ]; then
	xrandr --output DP1 --auto --off --output HDMI2 --auto --off --output DP2 --auto --pos 0x0 --scale 1.5x1.5 --output eDP1 --auto --pos 3840x0 --primary
else
	xrandr --output DP1 --auto --off --output DP2 --auto --off --output HDMI2 --auto --off --output eDP1 --auto --primary
fi
