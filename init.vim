set encoding=utf-8
scriptencoding utf-8

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')
" Make sure you use single quotes
" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
" Junegunn Choi's plugins
Plug '/usr/local/opt/fzf'                 " Add fzf to runtime path
Plug 'junegunn/fzf.vim'                   " Fuzzy Finder
Plug 'junegunn/vim-easy-align'            " Easily alignment
Plug 'junegunn/vim-github-dashboard'      " Browse GitHub events in Vim
Plug 'junegunn/vader.vim'                 " A simple Vimscript test framework

" Tim Pope's plugins
Plug 'tpope/vim-bundler'                  " Bundle support in vim
Plug 'tpope/vim-commentary'               " Comment stuff out
Plug 'tpope/vim-dadbod'                   " Modern database interface for Vim
Plug 'tpope/vim-dispatch'                 " Asynchronous build and test dispatcher
Plug 'tpope/vim-dotenv'                   " Basic support for .env and Procfile
Plug 'tpope/vim-endwise'                  " Add 'end' to ruby structures
Plug 'tpope/vim-eunuch'                   " Basic Unix commands
Plug 'tpope/vim-fugitive'                 " Easily manipulate Git(hub)
Plug 'tpope/vim-projectionist'            " Granular project configuration
Plug 'tpope/vim-rails'                    " Rails support in vim
Plug 'tpope/vim-rake'                     " It's like rails.vim without the rails
Plug 'tpope/vim-rbenv'                    " Rbenv within vim
Plug 'tpope/vim-repeat'                   " Advanced dot usage
Plug 'tpope/vim-rhubarb'                  " GitHub extension for fugitive.vim
Plug 'tpope/vim-speeddating'              " Increase date with <C-a>/<C-x>
Plug 'tpope/vim-surround'                 " Easily change Surround characters
Plug 'tpope/vim-unimpaired'               " Better buffers switching mapping

" Visualisation
Plug 'vim-airline/vim-airline'            " Airline
Plug 'vim-airline/vim-airline-themes'     " Themes for airline
Plug 'flazz/vim-colorschemes'             " Color Schemes
Plug 'ryanoasis/vim-devicons'             " Add file type glyphs/icons to popular Vim plugins
Plug 'airblade/vim-gitgutter'             " Shows a git diff in the gutter and stages/undoes hunks
" Plug 'qhuyduong/vim-ruby-conceal'         " Unicode goodness for Ruby code by using vim's `conceal` feature

" Editing
Plug 'w0rp/ale'                           " Code linting
Plug 'jiangmiao/auto-pairs'               " Auto insert or delete brackets, parens, quotes
Plug 'qhuyduong/dbext.vim'                " Database interaction from vim
Plug 'Shougo/deoplete.nvim'               " Autocompletion for neovim
Plug 'fszymanski/deoplete-emoji'          " Emoji autocompletion
Plug 'coderifous/textobj-word-column.vim' " Adds text-objects for word-based columns in Vim
Plug 'ntpeters/vim-better-whitespace'     " Remove trailing whitespaces
Plug 'alvan/vim-closetag'                 " Auto close (X)HTML tags
Plug 'easymotion/vim-easymotion'          " Enhanced vim motions
Plug 'tommcdo/vim-exchange'               " Easy text exchange operator for Vim
Plug 'ludovicchabant/vim-gutentags'       " Tags manager
Plug 'matze/vim-move'                     " Move lines/block
Plug 'kana/vim-textobj-user'              " Dependency
Plug 'kana/vim-textobj-entire'            " Add ie and ae textobj
Plug 'nelstrom/vim-textobj-rubyblock'     " A custom text object for selecting ruby blocks
Plug 'lucapette/vim-textobj-underscore'   " Underscore text-object for Vim
Plug 'AndrewRadev/splitjoin.vim'          " Better lines split/join

" Windows layout
Plug 'kassio/neoterm'                     " Wrapper of some vim/neovim's :terminal functions
Plug 'scrooloose/nerdtree'                " A tree explorer plugin for vim
Plug 'Xuyuanp/nerdtree-git-plugin'        " A plugin of NERDTree showing git status
Plug 'majutsushi/tagbar'                  " Tags sidebar
Plug 'benmills/vimux'                     " Interact with tmux within vim
Plug 'mhinz/vim-startify'                 " The fancy start screen for Vim
Plug 'janko-m/vim-test'                   " Run your tests at the speed of thought
Plug 'tmux-plugins/vim-tmux-focus-events' " Make terminal vim and tmux work better together
Plug 'christoomey/vim-tmux-navigator'     " Seamless navigation between tmux panes and vim splits
Plug 'mbbill/undotree'                    " View undo history

" Language: API Blueprint
Plug 'kylef/apiblueprint.vim'             " Syntax highlighting and linting for API Blueprint<

" Language: Ruby
Plug 'vim-ruby/vim-ruby'                  " Vim/Ruby Configuration Files

" Language: JavaScript
Plug 'carlitux/deoplete-ternjs'           " deoplete.nvim source for javascript
Plug 'mattn/emmet-vim'                    " Provides support for expanding abbreviations similar to emmet
Plug 'ternjs/tern_for_vim'                " Tern plugin for Vim
Plug 'pangloss/vim-javascript'            " Vastly improved Javascript indentation and syntax support in Vim
Plug 'mxw/vim-jsx'                        " React JSX syntax highlighting and indenting for vim
Plug 'moll/vim-node'                      " Like Rails.vim for Node

" Language: Json
Plug 'elzr/vim-json'                      " A better JSON for Vim

" Language: Proto
Plug 'qhuyduong/vim-proto'                " Google Proto Buffer syntax highlighting

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
set completeopt-=preview          " remove the horrendous preview window
set cursorline                    " highlight the current line for the cursor
set list
set listchars=tab:→\ ,eol:↵,trail:⋅,extends:❯,precedes:❮
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
set wildmode=full
set wildignorecase
set ttyfast " Send more characters at a given time.
set backspace=indent,eol,start
set undolevels=100
set ignorecase
set smartcase
set gdefault
set showmatch
set viminfo^=%
" Enable mouse if possible
if has('mouse')
  set mouse=a
endif

" Allow vim to set a custom font or color for a word
syntax enable

" Set the leader button
let g:mapleader = ','

" Define a group `Utilities` and initialize.
augroup Utilities
  autocmd!
  " Autosave buffers before leaving them
  autocmd BufLeave * silent! :wa
  " Remove trailing white spaces on save
  autocmd BufWritePre * StripWhitespace
augroup END

" Center the screen quickly
nnoremap <space> zz

" Key maps to emulate the "system clipboard" shortcuts
inoremap <C-v> <ESC>"+pa
vnoremap <C-c> "+y
vnoremap <C-x> "+d

"----------------------------------------------
" Colors
"----------------------------------------------
set background=dark
colorscheme wombat

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
" Skip the quickfix when navigating
augroup Quickfixes
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

"----------------------------------------------
" Plug 'Shougo/deoplete.nvim'
"----------------------------------------------
if has('nvim')
  " Enable deoplete on startup
  let g:deoplete#enable_at_startup = 1
endif

"----------------------------------------------
" Plug 'vim-airline/vim-airline'
"----------------------------------------------
" Show status bar by default.
set laststatus=2

" Set this. Airline will handle the rest.
let g:airline_theme = 'wombat'

" Enable top tabline.
let g:airline#extensions#tabline#enabled = 1

" Disable showing tabs in the tabline. This will ensure that the buffers are
" what is shown in the tabline at all times.
let g:airline#extensions#tabline#show_tabs = 0

" Show only file name in tabline
let g:airline#extensions#tabline#fnamemod = ':t'

" Show buffer index next to file name
let g:airline#extensions#tabline#buffer_nr_show = 1

" Enable powerline fonts.
let g:airline_powerline_fonts = 1

" Advanced separators (extra-powerline-symbols):
let g:airline_left_sep = "\uE0B4"
let g:airline_right_sep = "\uE0B6"

"----------------------------------------------
" Plug 'christoomey/vim-tmux-navigator'
"----------------------------------------------
" tmux will send xterm-style keys when its xterm-keys option is on.
if &term =~? '^screen'
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
" Plug 'easymotion/vim-easymotion'
"----------------------------------------------
" Enable support for bidirectional motions
nmap  <leader><leader>w <Plug>(easymotion-bd-w)
nmap <leader><leader>w <Plug>(easymotion-overwin-w)

let g:EasyMotion_do_mapping = 0 " Disable default mappings

" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap <Leader><Leader>s <Plug>(easymotion-overwin-f2)

" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
nmap <Leader>j <Plug>(easymotion-j)
nmap <Leader>k <Plug>(easymotion-k)

"----------------------------------------------
" Plug 'junegunn/fzf.vim'
"----------------------------------------------
nnoremap <c-p> :FZF<cr>

"----------------------------------------------
" Plugin: 'majutsushi/tagbar'
"----------------------------------------------
" Add shortcut for toggling the tag bar
nnoremap <F3> :TagbarToggle<cr>

"----------------------------------------------
" Plug 'scrooloose/nerdtree'
"----------------------------------------------
nnoremap <leader>d :NERDTreeToggle<cr>
nnoremap <F2> :NERDTreeToggle<cr>
nnoremap \ :Ag<cr>

" Files to ignore
let g:NERDTreeIgnore = [
      \ '\~$',
      \ '\.pyc$',
      \ '^\.DS_Store$',
      \ '^node_modules$',
      \ '^.ropeproject$',
      \ '^__pycache__$'
      \]

" Show hidden files by default.
let g:NERDTreeShowHidden = 1

" Allow NERDTree to change session root.
let g:NERDTreeChDirMode = 2

"----------------------------------------------
" Plug 'alvan/vim-closetag'
"----------------------------------------------
let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.erb,*.jsx'
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.erb'
let g:closetag_emptyTags_caseSensitive = 1
" Add > at current position without closing the current tag, default is ''
let g:closetag_close_shortcut = '<leader>>'

"----------------------------------------------
" Plug 'mattn/emmet-vim'
"----------------------------------------------
"let g:user_emmet_expandabbr_key='<Tab>'
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
" Plug 'ntpeters/vim-better-whitespace'
"----------------------------------------------
let g:better_whitespace_ctermcolor='green'

"----------------------------------------------
" Plug 'junegunn/vim-github-dashboard'
"----------------------------------------------
let g:github_dashboard = { 'username': $GITHUB_USER, 'password': $GITHUB_TOKEN }
let g:github_dashboard['position'] = 'right'

"----------------------------------------------
" Plug 'kassio/neoterm'
"----------------------------------------------
let g:neoterm_size='12'
let g:neoterm_default_mod = 'belowright'

augroup Terminals
  " Allow to navigation as normal
  autocmd TermOpen *neoterm* :tnoremap <buffer> <Esc> <C-\><C-n>
  autocmd TermOpen *neoterm* :tnoremap <buffer> <C-h> <C-\><C-n><C-w>h
  autocmd TermOpen *neoterm* :tnoremap <buffer> <C-k> <C-\><C-n><C-w>k
  autocmd TermOpen *neoterm* :tnoremap <buffer> <C-j> <C-\><C-n><C-w>j
  autocmd TermOpen *neoterm* :tnoremap <buffer> <C-l> <C-\><C-n><C-w>l
  " Exclude from buffer list
  autocmd TermOpen * set nobuflisted
augroup END

" Escape from terminal mode to normal mode
tnoremap <esc> <C-\><C-n>
nnoremap <silent> <C-t> :Ttoggle<cr>
" Toggle terminal from within terminal mode
tnoremap <silent> <C-t> <C-\><C-n>:Ttoggle<cr>

"----------------------------------------------
" Plug 'janko-m/vim-test'
"----------------------------------------------
let g:test#strategy = 'vimux'
let g:test#preserve_screen = 1

nnoremap <Leader>n :TestNearest<CR>
nnoremap <Leader>f :TestFile<CR>
nnoremap <Leader>s :TestSuite<CR>
nnoremap <Leader>l :TestLast<CR>
nnoremap <Leader>v :TestVisit<CR>

"----------------------------------------------
" Plug 'pangloss/vim-javascript'
"----------------------------------------------
let g:javascript_plugin_flow = 1

"----------------------------------------------
" Plug 'mxw/vim-jsx'
"----------------------------------------------
let g:jsx_ext_required = 1

"----------------------------------------------
" Plug 'benmills/vimux'
"----------------------------------------------
let g:VimuxHeight = '30'
let g:VimuxOrientation = 'h'

"----------------------------------------------
" Plug 'ludovicchabant/vim-gutentags'
"----------------------------------------------
let g:gutentags_cache_dir = get(g:, 'gutentags_cache_dir', expand('~/.cache/tags'))

"----------------------------------------------
" Plug 'junegunn/vim-easy-align'
"----------------------------------------------
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

"----------------------------------------------
" Plug 'w0rp/ale'
"----------------------------------------------
" Enable completion where available.
let g:ale_completion_enabled = 1

" Set this variable to 1 to fix files when you save them.
let g:ale_lint_on_save = 1

let g:ale_fixers = {
      \   'html': ['tidy'],
      \   'javascript': ['prettier-eslint'],
      \   'json': ['jq'],
      \   'ruby': ['rubocop'],
      \}

let g:ale_linters = {
      \   'html': ['tidy'],
      \   'javascript': ['eslint'],
      \   'json': ['jsonlint'],
      \   'ruby': ['rubocop'],
      \   'vim': ['vint'],
      \}

" Configure signs.
let g:ale_sign_error   = '✘'
let g:ale_sign_warning = '⚠'
highlight ALEErrorSign ctermbg=NONE ctermfg=red
highlight ALEWarningSign ctermbg=NONE ctermfg=yellow

let g:ale_html_tidy_options = '-q -i --show-errors 0'
let g:ale_json_jq_options = '--indent 2'

nnoremap <leader>a :ALEFix<CR>

"----------------------------------------------
" Plug 'mhinz/vim-startify'
"----------------------------------------------
let g:startify_change_to_vcs_root = 1

"----------------------------------------------
" Plug 'tpope/vim-dadbod'
"----------------------------------------------
let g:dadbod_manage_dbext = 1

"----------------------------------------------
" Plug 'mbbill/undotree'
"----------------------------------------------
nnoremap <F5> :UndotreeToggle<cr>

"----------------------------------------------
" Plug 'matze/vim-move'
"----------------------------------------------
let g:move_map_keys = 0
vmap <silent> <S-k> <Plug>MoveBlockUp
vmap <silent> <S-j> <Plug>MoveBlockDown

"----------------------------------------------
" Miscellaneous
"----------------------------------------------
" Grep word under cursor
nnoremap <leader>g :silent execute "grep! -R " . shellescape(expand("<cWORD>")) . " ."<cr>:copen<cr>

" Easy expansion of the active file directory
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Define a group `Languages` and initialize
augroup Languages
  autocmd!
augroup END

"----------------------------------------------
" Language: apiblueprint
"----------------------------------------------
autocmd Languages FileType apiblueprint set expandtab shiftwidth=4 softtabstop=4 tabstop=4

"----------------------------------------------
" Language: Bash
"----------------------------------------------
autocmd Languages FileType sh set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: gitcommit
"----------------------------------------------
autocmd Languages FileType gitcommit setlocal spell
autocmd Languages FileType gitcommit setlocal textwidth=80

"----------------------------------------------
" Language: gitconfig
"----------------------------------------------
autocmd Languages FileType gitconfig set noexpandtab shiftwidth=4 softtabstop=4 tabstop=4

"----------------------------------------------
" Language: HTML
"----------------------------------------------
autocmd Languages FileType html set expandtab shiftwidth=2 softtabstop=2 tabstop=2
"autocmd Languages FileType html imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

"----------------------------------------------
" Language: CSS
"----------------------------------------------
autocmd Languages FileType css set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: JavaScript
"----------------------------------------------
autocmd Languages FileType javascript* set expandtab shiftwidth=2 softtabstop=2 tabstop=2
"autocmd Languages FileType javascript.jsx imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

"----------------------------------------------
" Language: JSON
"----------------------------------------------
autocmd Languages FileType json set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: LESS
"----------------------------------------------
autocmd Languages FileType less set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: Make
"----------------------------------------------
autocmd Languages FileType make set noexpandtab shiftwidth=4 softtabstop=4 tabstop=4

"----------------------------------------------
" Language: Markdown
"----------------------------------------------
autocmd Languages FileType markdown setlocal spell
autocmd Languages FileType markdown set expandtab shiftwidth=4 softtabstop=4 tabstop=4 syntax=markdown

"----------------------------------------------
" Language: Ruby
"----------------------------------------------
autocmd Languages FileType ruby set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: SQL
"----------------------------------------------
autocmd Languages FileType sql set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: vimscript
"----------------------------------------------
autocmd Languages FileType vim set expandtab shiftwidth=2 softtabstop=2 tabstop=2

"----------------------------------------------
" Language: YAML
"----------------------------------------------
autocmd Languages FileType yaml set expandtab shiftwidth=2 softtabstop=2 tabstop=2
