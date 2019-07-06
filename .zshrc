# Begin profiling
# zmodload zsh/zprof

# History
export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt SHARE_HISTORY

export TERM=xterm-24bit
export LANG=en_US.UTF-8
export EDITOR=nvim
export LESS=-FXR
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

if [[ -n $INSIDE_EMACS ]]; then
  export EDITOR=emacsclient
  export VISUAL=$EDITOR
fi

# Emacs mode
bindkey -e

# Ruby
eval "$(rbenv init -)"

# Python
eval "$(pyenv init -)"
export PYENV_ROOT="$HOME/.pyenv"

# Node
export NVM_LAZY_LOAD=true

# Fzf
export FZF_DEFAULT_COMMAND='fd --type f --follow'

# Aliases
ls --color=auto &> /dev/null && alias ls='ls --color=auto'
alias ll='ls -lha'

# Essential
source ~/.zplug/init.zsh

# Zplug plugins
zplug "agkozak/agkozak-zsh-prompt"
zplug "hlissner/zsh-autopair", defer:2
# Async for zsh, used by pure
zplug "mafredri/zsh-async", from:github, defer:0
zplug "plugins/fzf", from:oh-my-zsh
zplug "plugins/git", from:oh-my-zsh
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

export AGKOZAK_PROMPT_CHAR=λ
export AGKOZAK_COLORS_PROMPT_CHAR=yellow

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Stop profiling
# zprof
