#!/bin/bash

delay_time="1"
cmd="systemctl suspend-then-hibernate"

while true ; do
	state=( $(</proc/acpi/button/lid/LID0/state) )
	[[ ${state[1]} = "closed" ]] && $cmd
	sleep $delay_time
done
