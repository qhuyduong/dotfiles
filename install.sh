#!/bin/sh

echo "Installing dependencies via Homebrew"
brew update && brew upgrade
brew install tree fzf curl rbenv the_silver_searcher jq \
						 tmuxinator-completion neovim zsh-completions \
						 zsh-syntax-highlighting emacs
brew remove ctags
brew install --HEAD universal-ctags/universal-ctags/universal-ctags

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

echo "Create symlinks emacs configs"
mv ~/.spacemacs ~/.spacemacs.bak
ln -s ~/dotfiles/.spacemacs ~/.spacemacs

echo "Clone spacemacs configs"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d -b develop
mkdir -p ~/.emacs.d/private/local/airline-themes
git clone https://github.com/qhuyduong/airline-themes.git ~/.emacs.d/private/local/airline-themes
mkdir -p ~/.emacs.d/private/local/flow-js2-mode
git clone https://github.com/Fuco1/flow-js2-mode.git ~/.emacs.d/private/local/flow-js2-mode

echo "Some oh-my-zsh tweaks"
cat <<EOF > ~/.oh-my-zsh/custom/vi-mode.zsh
# Remap ESC key
# bindkey "jj" vi-cmd-mode
# Return 'v' to its original function, i.e: visual mode
bindkey -M vicmd 'v' visual-mode
bindkey -M vicmd '^v' edit-command-line
EOF

cat <<EOF > ~/.oh-my-zsh/custom/zsh-autosuggestion.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=240'
EOF

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
