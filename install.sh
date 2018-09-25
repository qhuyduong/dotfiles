#!/bin/sh
echo "Installing Homebrew"
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

echo "Installing dependencies via Homebrew"
brew update && brew upgrade
brew install zsh tree fzf curl rbenv the_silver_searcher jq \
						 tmuxinator-completion neovim zsh-completions \
						 zsh-syntax-highlighting pyenv ispell mplayer \
             redis tmux postgres coreutils watchman
brew install --HEAD universal-ctags/universal-ctags/universal-ctags
brew tap d12frosted/emacs-plus
brew install emacs-plus --HEAD

echo "Installing oh-my-zsh"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

echo "Clone my dotfiles repository"
git clone https://github.com/qhuyduong/dotfiles.git ~/dotfiles

echo "Create symlinks zsh configs"
mv ~/.zshrc ~/.zshrc.bak
ln -s ~/dotfiles/.zshrc ~/.zshrc

echo "Create symlinks tmux configs"
mv ~/.tmux.conf ~/.tmux.conf.bak
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/.tmux.conf.local ~/.tmux.conf.local

echo "Create symlinks nvim configs"
mkdir -p ~/.config/nvim
mv ~/.config/nvim/init.vim ~/.config/nvim/init.vim.bak
ln -s ~/dotfiles/init.vim ~/.config/nvim/init.vim
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo "Create symlinks emacs configs"
mv ~/.spacemacs ~/.spacemacs.bak
ln -s ~/dotfiles/.spacemacs ~/.spacemacs

echo "Clone spacemacs configs"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d -b develop
mkdir -p ~/.emacs.d/private/local/airline-themes
git clone https://github.com/qhuyduong/airline-themes.git ~/.emacs.d/private/local/airline-themes
mkdir -p ~/.emacs.d/private/local/flow-js2-mode
git clone https://github.com/Fuco1/flow-js2-mode.git ~/.emacs.d/private/local/flow-js2-mode
mkdir -p ~/.emacs.d/private/local/nord-theme
git clone https://github.com/visigoth/nord-emacs.git ~/.emacs.d/private/local/nord-theme

echo "Some oh-my-zsh tweaks"
cat <<EOF > ~/.oh-my-zsh/custom/vi-mode.zsh
# Remap ESC key
# bindkey "jj" vi-cmd-mode
# Return 'v' to its original function, i.e: visual mode
bindkey -M vicmd 'v' visual-mode
bindkey -M vicmd '^v' edit-command-line
EOF

git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
cat <<EOF > ~/.oh-my-zsh/custom/zsh-autosuggestion.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=240'
EOF

echo "Configure fzf"
$(brew --prefix)/opt/fzf/install

echo "Some ctags config"
mkdir -p ~/.ctags.d
cat <<EOF > ~/.ctags.d/global.ctags
-R
--exclude=.git
--exclude=log
--exclude=node_modules
--exclude=dist
--tag-relative=yes
--exclude=*.min.js
--exclude=jquery.*.js
--exclude=jquery-*.js
--exclude=jquery*.js
--exclude=*jquery.js
--exclude=react_bundle.beta.js
--exclude=react_bundle.main.js
--exclude=*.css
--exclude=vendor
--exclude=spec
--exclude=__mocks__
--exclude=test
--exclude=__tests__
--exclude=__template__
--exclude=build
--exclude=manifest.json
--exclude=eh-code-quality.json
--exclude=app.json
--exclude=package.json
--exclude=latest
--exclude=public
--exclude=__mockData__
--exclude=*.sh
--exclude=*.svg
--exclude=*.png
--exclude=*.gif
--languages=-html
--extras=+q+f
--fields=+i+a+S+n
EOF

echo "Configure 24-bit terminal"
cat <<EOF > ~/xterm-24bit.terminfo
xterm-24bit|xterm with 24-bit direct color mode,
   use=xterm-256color,
   sitm=\E[3m,
   ritm=\E[23m,
   setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
   setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,

EOF
tic -x -o ~/.terminfo ~/xterm-24bit.terminfo

echo "Install iterm2 shell integration"
curl -L https://iterm2.com/shell_integration/install_shell_integration_and_utilities.sh | bash

echo "Install NVM"
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash

echo "Install Tmux plugins manager"
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
