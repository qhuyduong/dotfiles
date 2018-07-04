" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')
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
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-rhubarb'

" Vim Ruby
Plug 'vim-ruby/vim-ruby'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Color Schemes
Plug 'flazz/vim-colorschemes'
Plug 'frankier/neovim-colors-solarized-truecolor-only'

" NERD Commenter
" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'

" vim-ripgrep
Plug 'jremmen/vim-ripgrep'

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

Plug 'jiangmiao/auto-pairs'

Plug 'airblade/vim-gitgutter'

" ES2015 code snippets (Optional)
Plug 'epilande/vim-es2015-snippets', { 'for': '*javascript*' }
" React code snippets
Plug 'epilande/vim-react-snippets', { 'for': '*javascript*' }
" Ultisnips
Plug 'SirVer/ultisnips', { 'for': '*javascript*' }
Plug 'carlitux/deoplete-ternjs', { 'for': '*javascript*' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'

Plug 'mhinz/vim-startify'

Plug 'majutsushi/tagbar'

Plug 'terryma/vim-multiple-cursors'

Plug 'godlygeek/tabular'

Plug 'whatyouhide/vim-lengthmatters'

Plug 'moll/vim-node'

Plug 'lfilho/cosco.vim'

Plug 'elzr/vim-json'

Plug 'wakatime/vim-wakatime'

Plug 'Yggdroot/indentLine'

Plug 'romainl/vim-qf'

Plug 'kassio/neoterm'

Plug 'neomake/neomake'

Plug 'sbdchd/neoformat'

Plug 'janko-m/vim-test'

Plug 'easymotion/vim-easymotion'

Plug 'kylef/apiblueprint.vim'

Plug 'ryanoasis/vim-devicons'

Plug 'vimwiki/vimwiki'

Plug 'plasticboy/vim-markdown'

Plug 'christoomey/vim-tmux-navigator'
"
" Initialize plugin system
call plug#end()

"----------------------------------------------
" General settings
"----------------------------------------------
filetype plugin indent on
set autoindent                    " take indent for new line from previous line
set smartindent                   " enable smart indentation
set autoread                      " reload file if the file changes on the disk
set autowrite                     " write when switching buffers
set autowriteall                  " write on :quit
" yank to clipboard
"if has("clipboard")
"  set clipboard=unnamed " copy to the system clipboard
"  if has("unnamedplus") " X11 support
"    set clipboard+=unnamedplus
"  endif
"endif
set completeopt-=preview          " remove the horrendous preview window
set cursorline                    " highlight the current line for the cursor
set encoding=utf-8
set list
set listchars=tab:→\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
set showbreak=↪
set splitright
set nospell                       " disable spelling
set noswapfile                    " disable swapfile usage
set nowrap
set noerrorbells                  " No bells!
set novisualbell                  " I said, no bells!
set number                        " show number ruler
set relativenumber                " show relative numbers in the ruler
set ruler
set formatoptions=tcqronj         " set vims text formatting options
set expandtab                     " expands tabs to spaces
set softtabstop=2
set tabstop=2
set shiftwidth=2
set title                         " let vim set the terminal title
set updatetime=100                " redraw the status bar often
set nomodeline " don't use modeline (no. of lines in file containing vim config) (security)
set scrolloff=3
set showmode
set showcmd
set hidden " can put buffer to the background without writing to disk, will remember history/marks.
set wildmenu
set wildmode=list:longest,full
set ttyfast " Send more characters at a given time.
set backspace=indent,eol,start
set undolevels=100
set ignorecase
set smartcase
set gdefault
set showmatch

" Enable mouse if possible
if has('mouse')
    set mouse=a
endif

" Allow vim to set a custom font or color for a word
syntax enable

" Set the leader button
let mapleader = ','

" Autosave buffers before leaving them
autocmd BufLeave * silent! :wa

" Remove trailing white spaces on save
autocmd BufWritePre * StripWhitespace

" Center the screen quickly
nnoremap <space> zz

"----------------------------------------------
" Colors
"----------------------------------------------
set background=dark
"colorscheme PaperColor
colorscheme hybrid_material

" Override the search highlight color with a combination that is easier to
" read. The default PaperColor is dark green backgroun with black foreground.
"
" Reference:
" - http://vim.wikia.com/wiki/Xterm256_color_names_for_console_Vim
"highlight Search guibg=DeepPink4 guifg=White ctermbg=53 ctermfg=White

"----------------------------------------------
" Searching
"----------------------------------------------
set incsearch                     " move to match as you type the search query
set hlsearch                      " disable search result highlighting

if has('nvim')
  set inccommand=split          " enables interactive search and replace
endif

" Clear search highlights
nnoremap <Esc> :noh<CR><Esc>

" These mappings will make it so that going to the next one in a search will
" center on the line it's found in.
nnoremap n nzzzv
nnoremap N Nzzzv

"----------------------------------------------
" Navigation
"----------------------------------------------
" Esc remap
inoremap jj <Esc>

" Disable arrow keys
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

" Move between buffers with Shift + arrow key...
nnoremap <S-l> :bprevious<cr>
nnoremap <S-h> :bnext<cr>

" ... but skip the quickfix when navigating
augroup qf
  autocmd!
  autocmd FileType qf set nobuflisted
augroup END

" Fix some common typos
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Qall qall

"----------------------------------------------
" Splits
"----------------------------------------------
" Create horizontal splits below the current window
set splitbelow
set splitright

" Creating splits
nnoremap <leader>v :vsplit<cr>
nnoremap <leader>h :split<cr>

" Closing splits
nnoremap <leader>q :close<cr>

"----------------------------------------------
" Plugin: 'Shougo/deoplete.nvim'
"----------------------------------------------
if has('nvim')
    " Enable deoplete on startup
    let g:deoplete#enable_at_startup = 1
endif

" Disable deoplete when in multi cursor mode
function! Multiple_cursors_before()
    let b:deoplete_disable_auto_complete = 1
endfunction

function! Multiple_cursors_after()
    let b:deoplete_disable_auto_complete = 0
endfunction

"----------------------------------------------
" Plugin: 'vim-airline/vim-airline'
"----------------------------------------------
" Show status bar by default.
set laststatus=2

" Set this. Airline will handle the rest.
let g:airline_theme = 'luna'

" Enable top tabline.
let g:airline#extensions#tabline#enabled = 1

" Disable showing tabs in the tabline. This will ensure that the buffers are
" what is shown in the tabline at all times.
let g:airline#extensions#tabline#show_tabs = 0

" Show only file name in tabline
let g:airline#extensions#tabline#fnamemod = ':t'

" Show buffer index next to file name
"let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1

" Enable powerline fonts.
let g:airline_powerline_fonts = 1

"----------------------------------------------
" Plugin: 'christoomey/vim-tmux-navigator'
"----------------------------------------------
" tmux will send xterm-style keys when its xterm-keys option is on.
if &term =~ '^screen'
  execute "set <xUp>=\e[1;*A"
  execute "set <xDown>=\e[1;*B"
  execute "set <xRight>=\e[1;*C"
  execute "set <xLeft>=\e[1;*D"
endif

" Tmux vim integration
let g:tmux_navigator_no_mappings = 1
let g:tmux_navigator_save_on_switch = 1

" Move between splits with ctrl+h,j,k,l
nnoremap <silent> <c-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <c-j> :TmuxNavigateDown<cr>
nnoremap <silent> <c-k> :TmuxNavigateUp<cr>
nnoremap <silent> <c-l> :TmuxNavigateRight<cr>
nnoremap <silent> <c-\> :TmuxNavigatePrevious<cr>

"----------------------------------------------
" Plugin: 'easymotion/vim-easymotion'
"----------------------------------------------
" Enable support for bidirectional motions
map  <leader><leader>w <Plug>(easymotion-bd-w)
nmap <leader><leader>w <Plug>(easymotion-overwin-w)

"----------------------------------------------
" Plugin: 'junegunn/fzf.vim'
"----------------------------------------------
nnoremap <c-p> :FZF<cr>

"----------------------------------------------
" Plugin: 'majutsushi/tagbar'
"----------------------------------------------
" Add shortcut for toggling the tag bar
nnoremap <F3> :TagbarToggle<cr>

"----------------------------------------------
" Plugin: 'plasticboy/vim-markdown'
"----------------------------------------------
" Disable folding
let g:vim_markdown_folding_disabled = 1

" Auto shrink the TOC, so that it won't take up 50% of the screen
let g:vim_markdown_toc_autofit = 1

"----------------------------------------------
" Plugin: 'neomake/neomake'
"----------------------------------------------
" Configure signs.
let g:neomake_error_sign   = {'text': '✖', 'texthl': 'NeomakeErrorSign'}
let g:neomake_warning_sign = {'text': '∆', 'texthl': 'NeomakeWarningSign'}
let g:neomake_message_sign = {'text': '➤', 'texthl': 'NeomakeMessageSign'}
let g:neomake_info_sign    = {'text': 'ℹ', 'texthl': 'NeomakeInfoSign'}

"Tries to find eslint's binary locally, fallback to globally installed
if executable($PWD .'/node_modules/eslint/bin/eslint.js')
  let s:eslint_path = $PWD .'/node_modules/eslint/bin/eslint.js'
else
  let s:eslint_path = 'eslint'
endif
" eslint maker
let g:neomake_javascript_eslint_maker = {
      \ 'args': ['--env', 'es6', '-f', 'compact'],
      \ 'errorformat': '%E%f: line %l\, col %c\, Error - %m,%W%f: line %l\, col %c\, Warning - %m'
      \ }
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_json_enabled_makers = ['jsonlint']
let g:neomake_javascript_eslint_exe = s:eslint_path

"----------------------------------------------
" Plugin: 'scrooloose/nerdtree'
"----------------------------------------------
nnoremap <leader>d :NERDTreeToggle<cr>
nnoremap <F2> :NERDTreeToggle<cr>
nnoremap \ :Ag<cr>

" Files to ignore
let NERDTreeIgnore = [
      \ '\~$',
      \ '\.pyc$',
      \ '^\.DS_Store$',
      \ '^node_modules$',
      \ '^.ropeproject$',
      \ '^__pycache__$'
      \]

" Close vim if NERDTree is the only opened window.
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Show hidden files by default.
let NERDTreeShowHidden = 1

" Allow NERDTree to change session root.
let g:NERDTreeChDirMode = 2

"----------------------------------------------
" Plugin: 'vimwiki/vimwiki'
"----------------------------------------------
" Path to wiki
let g:vimwiki_list = [{
      \ 'path': '~/Notes/vimwiki',
      \ 'syntax': 'markdown',
      \ 'ext': '.md'}]

au FileType vimwiki set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Plugin: 'terryma/vim-multiple-cursors'
"----------------------------------------------
let g:multi_cursor_next_key='<C-n>'
let g:multi_cursor_skip_key='<C-b>'

"----------------------------------------------
" Plugin: 'alvan/vim-closetag'
"----------------------------------------------
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.erb,*.jsx"
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.erb'
let g:closetag_emptyTags_caseSensitive = 1
" Add > at current position without closing the current tag, default is ''
let g:closetag_close_shortcut = '<leader>>'

"----------------------------------------------
" Plugin: 'mattn/emmet-vim'
"----------------------------------------------
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

"----------------------------------------------
" Plugin: 'lfilho/cosco.vim'
"----------------------------------------------
let g:cosco_ignore_comment_lines = 1

"----------------------------------------------
" Plugin: 'ntpeters/vim-better-whitespace'
"----------------------------------------------
let g:better_whitespace_ctermcolor='green'

"----------------------------------------------
" Plugin: 'junegunn/vim-github-dashboard'
"----------------------------------------------
let g:github_dashboard = { 'username': 'qhuyduong', 'password': $GITHUB_TOKEN }
let g:github_dashboard['position'] = 'right'

"----------------------------------------------
" Plugin: 'kassio/neoterm'
"----------------------------------------------
let g:neoterm_default_mod = 'split'
" Useful maps
" hide/close terminal
nnoremap <Leader>x :Ttoggle<cr>

"----------------------------------------------
" Plugin: 'janko-m/vim-test'
"----------------------------------------------
nnoremap <Leader>n :TestNearest<CR>
nnoremap <Leader>f :TestFile<CR>
nnoremap <Leader>s :TestSuite<CR>
nnoremap <Leader>l :TestLast<CR>
nnoremap <Leader>v :TestVisit<CR>

"----------------------------------------------
" Plugin: 'T.B.D'
"----------------------------------------------

"----------------------------------------------
" Miscellaneous
"----------------------------------------------
" Generate js ctags file
function! Jsctags()
  :!find . -type f -iregex ".*\.js$" -not -path "./node_modules/*" -exec jsctags {} -f \; | sed '/^$/d' | LANG=C sort > tags
endfunction
" Buffers switching
noremap <C-j> <C-w>j<C-w>_
noremap <C-k> <C-w>k<C-w>_
" search remap
nnoremap / /\v
vnoremap / /\v
" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
let g:javascript_plugin_flow = 1
let g:jsx_ext_required = 0

"----------------------------------------------
" Language: apiblueprint
"----------------------------------------------
au FileType apiblueprint set expandtab shiftwidth=4 softtabstop=4 tabstop=4

"----------------------------------------------
" Language: Bash
"----------------------------------------------
au FileType sh set noexpandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: gitcommit
"----------------------------------------------
au FileType gitcommit setlocal spell
au FileType gitcommit setlocal textwidth=80

"----------------------------------------------
" Language: gitconfig
"----------------------------------------------
au FileType gitconfig set noexpandtab shiftwidth=4 softtabstop=4 tabstop=4

"----------------------------------------------
" Language: HTML
"----------------------------------------------
au FileType html set expandtab shiftwidth=2 softtabstop=2 tabstop=2
au FileType html imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

"----------------------------------------------
" Language: CSS
"----------------------------------------------
au FileType css set expandtab shiftwidth=2 softtabstop=2 tabstop=2
au FileType css autocmd BufWritePost * Neomake
au FileType css autocmd BufWritePost * Neoformat
au FileType css nmap <silent> <Leader>; <Plug>(cosco-commaOrSemiColon)
au FileType css imap <silent> <Leader>; <c-o><Plug>(cosco-commaOrSemiColon)

"----------------------------------------------
" Language: JavaScript
"----------------------------------------------
au FileType javascript set expandtab shiftwidth=2 softtabstop=2 tabstop=2
au FileType javascript autocmd BufWritePost * Neomake
au FileType javascript autocmd BufWritePost * Neoformat
au FileType javascript imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
au FileType javascript nmap <silent> <Leader>; <Plug>(cosco-commaOrSemiColon)
au FileType javascript imap <silent> <Leader>; <c-o><Plug>(cosco-commaOrSemiColon)

"----------------------------------------------
" Language: JSON
"----------------------------------------------
au FileType json set expandtab shiftwidth=2 softtabstop=2 tabstop=2
au FileType json autocmd BufWritePost * Neomake
au FileType json autocmd BufWritePost * Neoformat

"----------------------------------------------
" Language: LESS
"----------------------------------------------
au FileType less set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: Make
"----------------------------------------------
au FileType make set noexpandtab shiftwidth=4 softtabstop=4 tabstop=4

"----------------------------------------------
" Language: Markdown
"----------------------------------------------
au FileType markdown setlocal spell
au FileType markdown set expandtab shiftwidth=4 softtabstop=4 tabstop=4 syntax=markdown
au FileType markdown autocmd BufWritePost * Neomake
au FileType markdown autocmd BufWritePost * Neoformat

"----------------------------------------------
" Language: Ruby
"----------------------------------------------
au FileType ruby set expandtab shiftwidth=2 softtabstop=2 tabstop=2

" Enable neomake for linting.
au FileType ruby autocmd BufWritePost * Neomake
au FileType ruby autocmd BufWritePost * Neoformat

"----------------------------------------------
" Language: SQL
"----------------------------------------------
au FileType sql set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: vimscript
"----------------------------------------------
au FileType vim set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: YAML
"----------------------------------------------
au FileType yaml set expandtab shiftwidth=2 softtabstop=2 tabstop=2