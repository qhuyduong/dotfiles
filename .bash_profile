_isarch=false
[[ -f /etc/arch-release ]] && _isarch=true

if $_isarch; then
    tput_cmd=/bin/tput
    dircolors_cmd=/bin/dircolors
else
    tput_cmd=/usr/bin/tput
    dircolors_cmd=/usr/bin/dircolors
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x "$tput_cmd" ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
#    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '
	PS1='\[\e[1;32m\]┌─ \u@\h\[\e[m\]: \[\e[0;36m\]\w\[\e[m\]\n\[\e[1;32m\]└> \[\e[0m\]'
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# Aliases for MAC OS X
alias ll='ls -lha'
alias connect-sw7='ssh -XY hduong@10.38.12.21'
alias connect-sw4='ssh -XY hduong@10.38.12.20'
alias connect-pc='ssh -XY hduong@10.38.6.98'

# Bash completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

export EDITOR=vim

source ~/macbook_ascii.txt

export ARCH=arm
export CROSS_COMPILE=arm-none-eabi-

# Finished adapting your PATH environment variable for use with MacPorts.
[[ -n $TMUX ]] && PROMPT_COMMAND='echo -ne "\033k\033\0134\033k`basename ${PWD}`\033\0134"'
