#!/bin/bash

# from https://wiki.archlinux.org/index.php/Acpid#Laptop_Monitor_Power_Off
export XAUTHORITY=$(ps -C Xorg -f --no-header | sed -n 's/.*-auth //; s/ -[^ ].*//; p')

displaynum=`ls /tmp/.X11-unix/* | sed s#/tmp/.X11-unix/X##`
export DISPLAY=:$displaynum

# Typing with incredible speed
xset r rate 185 100
