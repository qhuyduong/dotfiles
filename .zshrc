# Begin profiling
# zmodload zsh/zprof

# History
export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt SHARE_HISTORY

export TERM=xterm-256color
export LANG=en_US.UTF-8
export EDITOR='nvim'
export LESS='-FXR'

if [[ -n $INSIDE_EMACS ]]; then
  export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=2'
  export EDITOR=emacsclient
  export VISUAL=$EDITOR
fi

# Ruby
eval "$(rbenv init -)"

# Python
eval "$(pyenv init -)"
export PYENV_ROOT="$HOME/.pyenv"

# Node
export NVM_LAZY_LOAD=true

# Fzf
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git --skip-vcs-ignores -f -g ""'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Aliases
ls --color=auto &> /dev/null && alias ls='ls --color=auto'
alias ll='ls -lha'

# Essential
source ~/.zplug/init.zsh

# Zplug plugins
zplug "agkozak/agkozak-zsh-prompt"
zplug "b4b4r07/enhancd", use:init.sh
# Async for zsh, used by pure
zplug "mafredri/zsh-async", from:github, defer:0
zplug 'plugins/fzf', from:oh-my-zsh
zplug 'plugins/git', from:oh-my-zsh
zplug 'plugins/vi-mode', from:oh-my-zsh
# Load completion library for those sweet [tab] squares
zplug "lib/completion", from:oh-my-zsh
zplug "lukechilds/zsh-nvm"
# Syntax highlighting for commands, load last
zplug "zplug/zplug"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:3

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

# Remap ESC key
# Return 'v' to its original function, i.e: visual mode
bindkey -M vicmd 'v' visual-mode
bindkey -M vicmd '^v' edit-command-line

# Stop profiling
# zprof
