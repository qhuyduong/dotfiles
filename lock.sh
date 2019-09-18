#!/bin/sh

TEXT='#c3e88dff' # text
BACKGROUND='#292d3eff' # background
WRONG='#f07178ff'
VERIFY='#82aaffff'

i3lock \
  --color="$(echo $BACKGROUND | cut -c1-7)" \
  --insidecolor=$BACKGROUND \
  --insidevercolor=$VERIFY  \
  --insidewrongcolor=$WRONG \
  --ringcolor=$TEXT         \
  --ringvercolor=$VERIFY    \
  --ringwrongcolor=$WRONG   \
  --verifcolor=$TEXT        \
  --wrongcolor=$TEXT        \
  --timecolor=$TEXT         \
  --datecolor=$TEXT         \
  --layoutcolor=$TEXT       \
  --screen=1                \
  --clock                   \
  --indicator               \
  --timestr="%H:%M"         \
  --datestr="%A, %m"
