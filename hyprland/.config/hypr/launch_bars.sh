#!/bin/bash

if [[ ! $(pidof eww) ]]; then
	eww daemon
	sleep 1
fi

monitors=($(hyprctl monitors -j | jq -r '.[] | .id'))
for i in "${!monitors[@]}"; do
	eww open-many bar$i
done

socat -u UNIX-CONNECT:/tmp/hypr/"$HYPRLAND_INSTANCE_SIGNATURE"/.socket2.sock - | rg --line-buffered "monitoradded|monitorremoved" | while read -r line; do
	monitors=($(hyprctl monitors -j | jq -r '.[] | .id'))
	for i in "${!monitors[@]}"; do
		eww open-many --restart bar$i
	done
done
