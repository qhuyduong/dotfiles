# Begin profiling
# zmodload zsh/zprof

# History
export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt SHARE_HISTORY

export TERM=xterm-24bit
export LANG=en_US.UTF-8
export EDITOR='nvim'
export LESS=-FXR
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

# Emacs mode
bindkey -e

# Fzf
export FZF_DEFAULT_COMMAND='fd --type f --follow'

# Aliases
ls --color=auto &> /dev/null && alias ls='ls --color=auto'
alias ll='ls -lha'
alias e=$EDITOR

function bgkill() {
  kill -9 $(jobs -l | head -1 | awk '{print $3}')
}

# Essential
source ~/.zplug/init.zsh

# Zplug plugins
zplug "djui/alias-tips"
zplug "hlissner/zsh-autopair", defer:2
# Async for zsh, used by pure
zplug "mafredri/zsh-async", from:github, defer:0
zplug "plugins/fzf", from:oh-my-zsh
zplug "plugins/git", from:oh-my-zsh
zplug "popstas/zsh-command-time"
# Load completion library for those sweet [tab] squares
zplug "lib/completion", from:oh-my-zsh
# Syntax highlighting for commands, load last
zplug "zplug/zplug"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:3
zplug "specious/bender", as:theme
zplug "kiurchv/asdf.plugin.zsh", defer:2

# Actually install plugins, prompt user input
if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  else
    echo
  fi
fi

# Then, source plugins and add commands to $PATH
zplug load

eval "$(direnv hook zsh)"

# Stop profiling
# zprof
