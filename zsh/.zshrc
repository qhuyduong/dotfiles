# Fig pre block. Keep at the top of this file.
[[ -f "$HOME/.fig/shell/zshrc.pre.zsh" ]] && builtin source "$HOME/.fig/shell/zshrc.pre.zsh"
# Begin profiling
# zmodload zsh/zprof

# Emacs mode
bindkey -e

# History
export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
# Appends every command to the history file once it is executed
setopt inc_append_history
# Reloads the history whenever you use it
setopt share_history

export TERM=xterm-256color
export LANG=en_US.UTF-8
export FZF_DEFAULT_COMMAND='rg --files  --hidden --follow --glob "!{.git, node_modules}"'

# Aliases
ls --color=auto &> /dev/null && alias ls='ls --color=auto'
alias ll='ls -lha'

function clion() {
  /opt/clion-*/bin/clion.sh "$@" > /dev/null 2>&1 &
}

function bgkill() {
  kill -9 $(jobs -l | head -1 | awk '{print $3}')
}

# Essential
source ~/.zplug/init.zsh

# Zplug plugins
zplug "djui/alias-tips"
zplug "hlissner/zsh-autopair", defer:2
zplug "kiurchv/asdf.plugin.zsh", defer:2
zplug "lib/completion", from:oh-my-zsh
zplug "mafredri/zsh-async", from:github, defer:0
zplug "plugins/fzf", from:oh-my-zsh
zplug "plugins/gh", from:oh-my-zsh
zplug "plugins/git", from:oh-my-zsh
zplug "popstas/zsh-command-time"
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
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

fortune | cowsay -pn

# Stop profiling
# zprof

# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/zshrc.post.zsh" ]] && builtin source "$HOME/.fig/shell/zshrc.post.zsh"
