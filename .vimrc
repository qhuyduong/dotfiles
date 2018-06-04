set nocompatible " required
filetype off     " required
set expandtab tabstop=2 softtabstop=2 shiftwidth=2 " default indentation

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required

Plugin 'gmarik/Vundle.vim'

" Add all your plugins here (note older versions of Vundle used Bundle instead of Plugin)
Plugin 'tpope/vim-rails'
" Fugitive
Plugin 'tpope/vim-fugitive'
" gem-ctags
Plugin 'tpope/gem-ctags'
" bundler.vim
Plugin 'tpope/vim-bundler'
" Surround
Plugin 'tpope/vim-surround'
" vim-rbenv
Plugin 'tpope/vim-rbenv'
" vim-rake
Plugin 'tpope/vim-projectionist'
Plugin 'tpope/vim-rake'

" Vim Ruby
Plugin 'vim-ruby/vim-ruby'

" Airline
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" CtrlP
Plugin 'ctrlpvim/ctrlp.vim'

" Color Schemes
Plugin 'flazz/vim-colorschemes'
Plugin 'altercation/vim-colors-solarized'

" NERD Commenter
Plugin 'scrooloose/NERDTree'
Plugin 'scrooloose/NERDCommenter'

" vim-ripgrep
Plugin 'jremmen/vim-ripgrep'

" Syntastic
Plugin 'vim-syntastic/syntastic'

" FzF
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'

" vim-closetag
Plugin 'alvan/vim-closetag'

" vim-codequery
Plugin 'Shougo/unite.vim'
Plugin 'devjoe/vim-codequery'

" vim-jsbeautify
Plugin 'maksimr/vim-jsbeautify'

" vim-trailing-whitespace
Plugin 'ntpeters/vim-better-whitespace'

" All of your Plugins must be added before the following line call vundle#end()  " required
call vundle#end()

filetype plugin indent on    " required

" Global settings
set term=xterm
set t_Co=256
let &t_AB="\e[48;5;%dm"
let &t_AF="\e[38;5;%dm"
syntax enable
set modelines=0
set number
set ruler
set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest,full
set visualbell
set ttyfast
set backspace=indent,eol,start
set laststatus=2
" set relativenumber
set cursorline
set undolevels=100
set title
set noerrorbells
set noswapfile
set nobackup
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
set wrap
set fo+=tc
set textwidth=80
set colorcolumn=80
" yank to clipboard
if has("clipboard")
  set clipboard=unnamed " copy to the system clipboard

  if has("unnamedplus") " X11 support
    set clipboard+=unnamedplus
  endif
endif
let mapleader = ","

" CtrlP settings
if executable('rg')
  set grepprg=rg\ --color=never
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
endif
let g:ctrlp_custom_ignore = {
      \ 'dir': '\.git$\|\.yardoc\|bower_components|node_modules|public$|log\|tmp$',
      \ 'file': '\.so$\|\.dat$|\.DS_Store$'
      \ }

" Airlines settings
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme = 'luna'

" Colour settings
"colorscheme solarized
colorscheme hybrid_material
set background=dark
"let g:solarized_termcolors = 256
"let g:solarized_termtrans = 1

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_javascript_checkers = ['']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_scss_checkers = [ 'sass_lint' ]
let g:syntastic_sass_sass_args = '-I ' . getcwd()

" To have NERDTree always open on startup
let g:nerdtree_tabs_open_on_console_startup = 1

"" FZF
"let g:fzf_action = { 'enter': 'tabedit' }

"" Closetag
" filetypes like xml, html, xhtml, ...
" These are the file types where this plugin is enabled.
let g:closetag_filetypes = 'html,xhtml,phtml,html.erb'
" Add > at current position without closing the current tag, default is ''
let g:closetag_close_shortcut = '<leader>>'

" Set tags option
set tags=./ruby_tags;/

" File types
augroup file_types
  autocmd!
  autocmd BufRead,BufNewFile *.fdoc set filetype=yaml
  autocmd BufRead,BufNewFile *.md set filetype=markdown
  autocmd BufRead,BufNewFile *.txt set filetype=markdown
  autocmd BufRead,BufNewFile *.module set filetype=php
  autocmd BufRead,BufNewFile *.install set filetype=php
  autocmd BufRead,BufNewFile *.test set filetype=php
  autocmd BufRead,BufNewFile *.inc set filetype=php
  autocmd BufRead,BufNewFile *.profile set filetype=php
  autocmd BufRead,BufNewFile *.view set filetype=php
  autocmd BufNewFile,BufRead *.less set filetype=less
  autocmd BufRead,BufNewFile *.ts set ft=typescript syntax=typescript
  autocmd BufRead,BufNewFile *.es6 set ft=javascript syntax=javascript
  autocmd BufRead,BufNewFile *.json set ft=json syntax=javascript
  autocmd BufRead,BufNewFile *.twig set ft=htmldjango
  autocmd BufRead,BufNewFile *.rabl set ft=ruby
  autocmd BufRead,BufNewFile *.jade set ft=jade
  autocmd BufRead,BufNewFile *.py set expandtab tabstop=4 softtabstop=4 shiftwidth=4
  autocmd BufRead,BufNewFile *.js set expandtab tabstop=2 softtabstop=2 shiftwidth=2 ft=javascript syntax=javascript
  autocmd BufRead,BufNewFile *.html,*.erb set expandtab tabstop=2 softtabstop=2 shiftwidth=2 ft=html syntax=html
  autocmd BufRead,BufNewFile *.xml set expandtab tabstop=2 softtabstop=2 shiftwidth=2
  autocmd BufRead,BufNewFile *.css,*.scss set expandtab tabstop=2 softtabstop=2 shiftwidth=2 ft=css syntax=css
  autocmd BufRead,BufNewFile *.rb set expandtab tabstop=2 softtabstop=2 shiftwidth=2
augroup END

" Highlight trailing-whitespace
let g:better_whitespace_ctermcolor='green'

"""""""""" Keys mapping """"""""""
map <C-\> :NERDTreeToggle<CR>
" Buffers switching
nmap <C-l> :bnext<CR>
nmap <C-h> :bprev<CR>
map <C-j> <C-w>j<C-w>_
map <C-k> <C-w>k<C-w>_
" vim-ruby Omnicomplete
imap <C-@> <C-Space>
imap <C-Space> <C-x><C-o>
" search remap
nnoremap / /\v
vnoremap / /\v
" clear search
nnoremap <leader>c :noh<cr>
" NERDTree settings
nnoremap <leader>d :NERDTreeToggle<CR>
" FZF settings
nnoremap <leader><space> :FZF<cr>
" CtrlP settings
nnoremap <leader>. :CtrlPTag<cr>
" Disable arrow keys
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>
" Esc remap
inoremap jj <Esc>
" codequery
nnoremap <leader>\ :CodeQuery<CR>
nnoremap <leader><CR> :CodeQueryMakeDB<CR>
" jsbeautify
autocmd FileType javascript noremap <buffer> <leader>f :call JsBeautify()<cr>
autocmd FileType javascript vnoremap <buffer> <leader>f :call RangeJsBeautify()<cr>
" for json
autocmd FileType json noremap <buffer> <leader>f :call JsonBeautify()<cr>
autocmd FileType json vnoremap <buffer> <leader>f :call RangeJsonBeautify()<cr>
" for jsx
autocmd FileType jsx noremap <buffer> <leader>f :call JsxBeautify()<cr>
autocmd FileType jsx vnoremap <buffer> <leader>f :call RangeJsxBeautify()<cr>
" for html
autocmd FileType html noremap <buffer> <leader>f :call HtmlBeautify()<cr>
autocmd FileType html vnoremap <buffer> <leader>f :call RangeHtmlBeautify()<cr>
" for css or scss
autocmd FileType css noremap <buffer> <leader>f :call CSSBeautify()<cr>
autocmd FileType css vnoremap <buffer> <leader>f :call RangeCSSBeautify()<cr>
