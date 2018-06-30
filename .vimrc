" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
" Make sure you use single quotes
" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'
" FzF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-github-dashboard'

" vim-rails
Plug 'tpope/vim-rails'
" Fugitive
Plug 'tpope/vim-fugitive'
" gem-ctags
Plug 'tpope/gem-ctags'
" bundler.vim
Plug 'tpope/vim-bundler'
" Surround
Plug 'tpope/vim-surround'
" vim-rbenv
Plug 'tpope/vim-rbenv'
" vim-rake
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-rake'
" Add 'end' to ruby structures
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'

" Vim Ruby
Plug 'vim-ruby/vim-ruby'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Color Schemes
Plug 'flazz/vim-colorschemes'
Plug 'altercation/vim-colors-solarized'

" NERD Commenter
" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'

" vim-ripgrep
Plug 'jremmen/vim-ripgrep'

" Syntastic
Plug 'vim-syntastic/syntastic'

" vim-closetag
Plug 'alvan/vim-closetag'

" vim-trailing-whitespace
Plug 'ntpeters/vim-better-whitespace'

" vim-jsx
Plug 'pangloss/vim-javascript', { 'for': '*javascript*' }
Plug 'mxw/vim-jsx', { 'for': '*javascript*' }
Plug 'ternjs/tern_for_vim', { 'for': '*javascript*', 'do': 'npm install' }
" emmet-vim
Plug 'mattn/emmet-vim', { 'for': ['html', '*erb', '*javascript*'] }
" vim-jsbeautify
Plug 'maksimr/vim-jsbeautify', { 'for': '*javascript*' }

Plug 'jiangmiao/auto-pairs'

Plug 'airblade/vim-gitgutter'

" ES2015 code snippets (Optional)
Plug 'epilande/vim-es2015-snippets', { 'for': '*javascript*' }
" React code snippets
Plug 'epilande/vim-react-snippets', { 'for': '*javascript*' }
" Ultisnips
Plug 'SirVer/ultisnips', { 'for': '*javascript*' }

Plug 'mhinz/vim-startify'

Plug 'majutsushi/tagbar'

Plug 'terryma/vim-multiple-cursors'

Plug 'godlygeek/tabular'

Plug 'whatyouhide/vim-lengthmatters'

Plug 'Valloric/YouCompleteMe', { 'for': ['*javascript*', 'ruby'] }

Plug 'moll/vim-node'

Plug 'prettier/vim-prettier'

Plug 'lfilho/cosco.vim'

Plug 'elzr/vim-json'

" Initialize plugin system
call plug#end()

packadd! matchit

" Global settings
set expandtab tabstop=2 softtabstop=2 shiftwidth=2 " default indentation
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
"set textwidth=80
set colorcolumn=80
" yank to clipboard
if has("clipboard")
  set clipboard=unnamed " copy to the system clipboard

  if has("unnamedplus") " X11 support
    set clipboard+=unnamedplus
  endif
endif
let mapleader = ","

" Airlines settings
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
" Set this. Airline will handle the rest.
let g:airline#extensions#ale#enabled = 1
let g:airline_theme = 'luna'

" Colour settings
set background=dark
colorscheme hybrid_material
"colorscheme solarized
"let g:solarized_termcolors = 256
"let g:solarized_termtrans = 1

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_scss_checkers = [ 'sass_lint' ]
let g:syntastic_sass_sass_args = '-I ' . getcwd()

"Tries to find eslint's binary locally, fallback to globally installed
if executable($PWD .'/node_modules/eslint/bin/eslint.js')
  let s:eslint_path = $PWD .'/node_modules/eslint/bin/eslint.js'
else
  let s:eslint_path = 'eslint'
endif

let s:eslint_maker = {
      \ 'args': [' --no-color', '--format', 'compact', '--quiet'],
      \ 'errorformat': '%f: line %l\, col %c\, %m',
      \ }

" To have NERDTree always open on startup
let g:nerdtree_tabs_open_on_console_startup = 1

"" FZF
"let g:fzf_action = { 'enter': 'tabedit' }

"" Closetag
" filetypes like xml, html, xhtml, ...
" These are the file types where this plugin is enabled.
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.erb,*.jsx"
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.erb'
let g:closetag_emptyTags_caseSensitive = 1
" Add > at current position without closing the current tag, default is ''
let g:closetag_close_shortcut = '<leader>>'

let g:javascript_plugin_flow = 1
let g:jsx_ext_required = 0

" emmet settings
let g:user_emmet_expandabbr_key='<Tab>'
let g:user_emmet_settings = {
      \  'javascript.jsx' : {
      \      'extends' : 'jsx',
      \      'block_all_childless' : 1,
      \      'quote_char': "'",
      \  },
      \  'html': {
      \      'block_all_childless' : 1,
      \      'quote_char': "'",
      \  },
      \}

let g:cosco_ignore_comment_lines = 1

"prettier
"run prettier before saving
let g:prettier#autoformat = 0
let g:prettier#config#single_quote = 'true'
let g:prettier#config#bracket_spacing = 'true'
let g:prettier#config#jsx_bracket_same_line = 'false'

" Highlight trailing-whitespace
let g:better_whitespace_ctermcolor='green'

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
  autocmd BufRead,BufNewFile *.es6 set ft=javascript.jsx syntax=javascript.jsx
  autocmd BufRead,BufNewFile *.json set expandtab tabstop=4 softtabstop=4 shiftwidth=4 ft=json syntax=javascript
  autocmd BufRead,BufNewFile *.twig set ft=htmldjango
  autocmd BufRead,BufNewFile *.rabl set ft=ruby
  autocmd BufRead,BufNewFile *.jade set ft=jade
  autocmd BufRead,BufNewFile *.py set expandtab tabstop=4 softtabstop=4 shiftwidth=4
  autocmd BufRead,BufNewFile *.js set ft=javascript syntax=javascript
  autocmd BufRead,BufNewFile *.jsx set ft=javascript.jsx syntax=javascript.jsx
  autocmd BufRead,BufNewFile *.xml,*.html,*.erb set ft=html syntax=html
  autocmd BufRead,BufNewFile *.css,*.scss set ft=css syntax=css
  autocmd BufRead,BufNewFile *.rb set ft=ruby
augroup END

function! Jsctags()
  :!find . -type f -iregex ".*\.js$" -not -path "./node_modules/*" -exec jsctags {} -f \; | sed '/^$/d' | LANG=C sort > tags
endfunction

"""""""""" Keys mapping """"""""""
map <C-\> :NERDTreeToggle<CR>
" Buffers switching
nmap <C-l> :bnext<CR>
nmap <C-h> :bprev<CR>
map <C-j> <C-w>j<C-w>_
map <C-k> <C-w>k<C-w>_
" search remap
nnoremap / /\v
vnoremap / /\v
" clear search
nnoremap <leader>c :noh<cr>
" NERDTree settings
nnoremap <leader>d :NERDTreeToggle<CR>
" FZF settings
nnoremap <leader><space> :FZF<cr>
nnoremap \ :Ag<cr>
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
" js-beautify
" for javascript
autocmd FileType javascript noremap <buffer> <leader>f :call JsBeautify()<cr>
autocmd FileType javascript vnoremap <buffer> <leader>f :call RangeJsBeautify()<cr>
" for json
autocmd FileType json noremap <buffer> <leader>f :call JsonBeautify()<cr>
autocmd FileType json vnoremap <buffer> <leader>f :call RangeJsonBeautify()<cr>
" for jsx
autocmd FileType javascript.jsx noremap <buffer> <leader>f :call JsxBeautify()<cr>
autocmd FileType javascript.jsx vnoremap <buffer> <leader>f :call RangeJsxBeautify()<cr>
" for html
autocmd FileType html noremap <buffer> <leader>f :call HtmlBeautify()<cr>
autocmd FileType html vnoremap <buffer> <leader>f :call RangeHtmlBeautify()<cr>
" for css or scss
autocmd FileType css noremap <buffer> <leader>f :call CSSBeautify()<cr>
autocmd FileType css vnoremap <buffer> <leader>f :call RangeCSSBeautify()<cr>
" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
" emmet
autocmd FileType html,javascript.jsx imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
autocmd FileType javascript.*,css nmap <silent> <Leader>; <Plug>(cosco-commaOrSemiColon)
autocmd FileType javascript.*,css imap <silent> <Leader>; <c-o><Plug>(cosco-commaOrSemiColon)
