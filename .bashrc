# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

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

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x "$dircolors_cmd" ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi
stty erase ^?

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export PROMPT_COMMAND='echo -ne "\033k\033\0134\033k`basename ${PWD}`\033\0134"'

# vi alias
alias vi='/usr/bin/vim'
export EDITOR=vim

# screen alias
alias scrls='screen -ls'
alias scropen='screen -xS -R'

alias cls='clear'
alias hgrep='history | grep'
alias la='ls -A'
alias l='ls -CF'
alias gitdiff='git diff --no-index'
alias mcscope='find . -name '*.[chS]' -print > cscope.files; cscope -R -b -q -k; ctags -L cscope.files'

alias ll='ls -Alrt'
export P4PORT=hcmp4proxy01:1666
export P4DIFF=/usr/bin/diff
export P4USER=hduong
export P4PASSWD=amcc1234
export P4EDITOR=vim
export P4CLIENT=gitp4admin

export AARCH64_TOOLCHAIN_VER=8.0.12-le;
export ARCH=arm64;
export CROSS_COMPILE=aarch64-apm-linux-gnu-;
export PATH=/tools/arm/armv8/Theobroma/opt/apm-aarch64/$AARCH64_TOOLCHAIN_VER/bin:/AMCC/hduong/utilities/gcc-linaro-6.1.1-2016.08-x86_64_aarch64-linux-gnu/bin:/tools/perforce/p4/v2015.1/bin:~/opt/bin:$PATH
export QT_XKB_CONFIG_ROOT=/usr/lib/kbd/keymaps/xkb

alias arm32="export ARCH=arm;
	     export CROSS_COMPILE=arm-none-eabi-
             export PATH=/AMCC/hduong/opt/toolchains/gcc-arm-none-eabi-5_4-2016q3/bin:$PATH"

alias connect-sw4="ssh -Y hduong@10.38.12.20"
alias connect-sw7="ssh -Y hduong@10.38.12.21"
alias connect-sw8="ssh -Y hduong@10.38.12.23"
alias connect-pc="ssh -Y hduong@10.38.7.104"

alias testing06="ssh -Y hduong@10.38.12.56"
alias ser2tel="telnet 10.38.12.71"
alias ser2tel56="telnet 10.38.12.56"

alias opensource="cd /AMCC/hduong/opensource/"

alias vanilla="cd /AMCC/hduong/opensource/linux-stable/"

alias gotftp="cd /tftpboot/hduong/"

alias release="cd /hcm_software/ftproot/apmsw/"

alias workspace="cd /AMCC/hduong/"

source ~/macbook_ascii.sh

unset SSH_ASKPASS
export MAILCHECK=0
