#!/usr/bin/env bash

i3status | while :
do
  read line
  pomodoro=`i3-gnome-pomodoro status`
  if [ -z "$pomodoro" ]; then
    echo "$line"
  else
    echo "$pomodoro| $line"
  fi
done
