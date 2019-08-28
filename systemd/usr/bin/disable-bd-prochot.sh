#!/bin/bash

old_value=$(rdmsr 0x1fc)
echo "Old value:" $old_value
new_value=$((0x$old_value & 0xFFFFFFFE))
printf "New value: %x\n" $new_value
wrmsr -a 0x1fc $new_value
