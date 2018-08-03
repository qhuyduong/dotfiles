" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')
" Make sure you use single quotes
" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'                                          " Fuzzy Finder
Plug 'junegunn/vim-github-dashboard'

" Tim Pope's plugins
Plug 'tpope/vim-bundler'                                         " Bundle support in vim
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dadbod'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-dotenv'
Plug 'tpope/vim-endwise'                                         " Add 'end' to ruby structures
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'                                        " Easily manipulate Git(hub)
Plug 'tpope/vim-projectionist'                                   " vim-rake
Plug 'tpope/vim-rails'                                           " Rails support in vim
Plug 'tpope/vim-rake'
Plug 'tpope/vim-rbenv'                                           " Rbenv within vim
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-speeddating'                                     " Increase date with <C-a>/<C-x>
Plug 'tpope/vim-surround'                                        " Easily change Surround characters
Plug 'tpope/vim-unimpaired'                                      " Better buffers switching mapping

Plug 'vim-ruby/vim-ruby'                                         " Vim Ruby
Plug 'vim-airline/vim-airline'                                   " Airline
Plug 'vim-airline/vim-airline-themes'
Plug 'flazz/vim-colorschemes'                                    " Color Schemes
Plug 'scrooloose/nerdtree'
Plug 'jremmen/vim-ripgrep'                                       " vim-ripgrep
Plug 'alvan/vim-closetag'                                        " vim-closetag
Plug 'ntpeters/vim-better-whitespace'                            " vim-trailing-whitespace
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }          " vim-jsx
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'ternjs/tern_for_vim', { 'for': 'javascript', 'do': 'npm install' }
Plug 'mattn/emmet-vim', { 'for': ['html', 'erb', 'javascript'] } " emmet-vim
Plug 'jiangmiao/auto-pairs'
Plug 'airblade/vim-gitgutter'
Plug 'epilande/vim-es2015-snippets', { 'for': 'javascript' }     " ES2015 code snippets (Optional)
Plug 'epilande/vim-react-snippets', { 'for': 'javascript' }      " React code snippets
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }    " Ultisnips
Plug 'fszymanski/deoplete-emoji'
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'honza/vim-snippets'
Plug 'carlitux/deoplete-ternjs', { 'for': 'javascript' }
" Plug 'SirVer/ultisnips', { 'for': 'javascript' }
" Plug 'MarcWeber/vim-addon-mw-utils'
" Plug 'tomtom/tlib_vim'
" Plug 'garbas/vim-snipmate'
Plug 'mhinz/vim-startify'
Plug 'majutsushi/tagbar'
Plug 'godlygeek/tabular'
Plug 'moll/vim-node'
Plug 'lfilho/cosco.vim'
Plug 'elzr/vim-json'
Plug 'Yggdroot/indentLine'
Plug 'kassio/neoterm'
Plug 'janko-m/vim-test'
Plug 'easymotion/vim-easymotion'
Plug 'kylef/apiblueprint.vim', { 'for': 'apiblueprint' }
Plug 'ryanoasis/vim-devicons'
Plug 'plasticboy/vim-markdown'
Plug 'christoomey/vim-tmux-navigator'
Plug 'benmills/vimux'
Plug 'xolox/vim-misc'                                            " Dependency
Plug 'xolox/vim-session'                                         " Sessions manager
Plug 'ludovicchabant/vim-gutentags'                              " Tags manager
Plug 'kana/vim-textobj-user'                                     " Dependency
Plug 'kana/vim-textobj-entire'                                   " Add ie and ae textobj
Plug 'w0rp/ale'                                                  " Code linting
Plug 'qhuyduong/dbext.vim'                                       " Database interaction from vim
Plug 'mbbill/undotree'                                           " View undo history
Plug 'AndrewRadev/splitjoin.vim'                                 " Better lines split/join
Plug 'matze/vim-move'                                            " Move lines/block
Plug 'tommcdo/vim-exchange'                                      " Easy text exchange operator for Vim
Plug 'tmux-plugins/vim-tmux-focus-events'                        " Make terminal vim and tmux work better together

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
set wildmode=full
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
let mapleader = ','

" Autosave buffers before leaving them
autocmd BufLeave * silent! :wa

" Remove trailing white spaces on save
autocmd BufWritePre * StripWhitespace

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
colorscheme hybrid_material

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

" Disable arrow keys
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

" Skip the quickfix when navigating
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

"----------------------------------------------
" Plug 'Shougo/deoplete.nvim'
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
" Plug 'vim-airline/vim-airline'
"----------------------------------------------
" Show status bar by default.
set laststatus=2

" Set this. Airline will handle the rest.
let g:airline_theme = 'hybrid'

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
" Plug 'easymotion/vim-easymotion'
"----------------------------------------------
" Enable support for bidirectional motions
map  <leader><leader>w <Plug>(easymotion-bd-w)
nmap <leader><leader>w <Plug>(easymotion-overwin-w)

let g:EasyMotion_do_mapping = 0 " Disable default mappings

" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap <Leader><Leader>s <Plug>(easymotion-overwin-f2)

" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

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
" Plug 'plasticboy/vim-markdown'
"----------------------------------------------
" Disable folding
let g:vim_markdown_folding_disabled = 1

" Auto shrink the TOC, so that it won't take up 50% of the screen
let g:vim_markdown_toc_autofit = 1

"----------------------------------------------
" Plug 'scrooloose/nerdtree'
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

" Show hidden files by default.
let NERDTreeShowHidden = 1

" Allow NERDTree to change session root.
let g:NERDTreeChDirMode = 2

"----------------------------------------------
" Plug 'terryma/vim-multiple-cursors'
"----------------------------------------------
let g:multi_cursor_next_key='<C-n>'
let g:multi_cursor_skip_key='<C-b>'

"----------------------------------------------
" Plug 'alvan/vim-closetag'
"----------------------------------------------
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.erb,*.jsx"
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
" Plug 'lfilho/cosco.vim'
"----------------------------------------------
let g:cosco_ignore_comment_lines = 1

"----------------------------------------------
" Plug 'ntpeters/vim-better-whitespace'
"----------------------------------------------
let g:better_whitespace_ctermcolor='green'

"----------------------------------------------
" Plug 'junegunn/vim-github-dashboard'
"----------------------------------------------
let g:github_dashboard = { 'username': 'qhuyduong', 'password': $GITHUB_TOKEN }
let g:github_dashboard['position'] = 'right'

"----------------------------------------------
" Plug 'kassio/neoterm'
"----------------------------------------------
let g:neoterm_size='12'
let g:neoterm_default_mod = 'belowright'

" allow to navigation as normal
au TermOpen *neoterm* :tnoremap <buffer> <Esc> <C-\><C-n>
au TermOpen *neoterm* :tnoremap <buffer> <C-h> <C-\><C-n><C-w>h
au TermOpen *neoterm* :tnoremap <buffer> <C-k> <C-\><C-n><C-w>k
au TermOpen *neoterm* :tnoremap <buffer> <C-j> <C-\><C-n><C-w>j
au TermOpen *neoterm* :tnoremap <buffer> <C-l> <C-\><C-n><C-w>l

" augroup Term
"   autocmd!
"   " Always start in terminal mode in term buffers
"   autocmd TermOpen * startinsert
"   autocmd BufEnter term://* startinsert
"   autocmd BufLeave term://* stopinsert
" augroup END

" escape from terminal mode to normal mode
tnoremap <esc> <C-\><C-n>
nnoremap <silent> <C-t> :Ttoggle<cr>
" toggle terminal from within terminal mode
tnoremap <silent> <C-t> <C-\><C-n>:Ttoggle<cr>

"----------------------------------------------
" Plug 'janko-m/vim-test'
"----------------------------------------------
let g:test#strategy = "vimux"
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
let g:jsx_ext_required = 0

"----------------------------------------------
" Plug 'xolox/vim-session'
"----------------------------------------------
let g:session_autoload = 'no'
let g:session_autosave = 'no'

"----------------------------------------------
" Plug 'Shougo/neosnippet'
"----------------------------------------------
let g:neosnippet#enable_completed_snippet = 1
let g:neosnippet#enable_snipmate_compatibility = 1

" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <expr><TAB>
      \ pumvisible() ? "\<C-n>" :
      \ neosnippet#expandable_or_jumpable() ?
      \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif

"----------------------------------------------
" Plug 'benmills/vimux'
"----------------------------------------------
let g:VimuxHeight = "30"
let g:VimuxOrientation = "h"

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
" let g:ale_fix_on_save = 1

let g:ale_fixers = {
      \   'javascript': ['prettier'],
      \   'json': ['jq'],
      \   'ruby': ['rubocop'],
      \}

let g:ale_linters = {
      \   'javascript': ['eslint'],
      \   'json': ['jsonlint'],
      \   'ruby': ['rubocop'],
      \}

" Configure signs.
let g:ale_sign_error   = '✘'
let g:ale_sign_warning = '⚠'
highlight ALEErrorSign ctermbg=NONE ctermfg=red
highlight ALEWarningSign ctermbg=NONE ctermfg=yellow

let g:ale_json_jq_options = '--indent 4'

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
let g:move_key_modifier = 'S'

"----------------------------------------------
" Plug 'T.B.D'
"----------------------------------------------

"----------------------------------------------
" Miscellaneous
"----------------------------------------------
" Generate js ctags file
" function! Jsctags()
"   :!find . -type f -iregex ".*\.js$" -not -path "./node_modules/*" -exec jsctags {} -f \; | sed '/^$/d' | LANG=C sort > tags
" endfunction

" Grep word under cursor
nnoremap <leader>g :silent execute "grep! -R " . shellescape(expand("<cWORD>")) . " ."<cr>:copen<cr>

" Easy expansion of the active file directory
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

"----------------------------------------------
" Language: apiblueprint
"----------------------------------------------
au FileType apiblueprint set expandtab shiftwidth=4 softtabstop=4 tabstop=4

"----------------------------------------------
" Language: Bash
"----------------------------------------------
au FileType sh set expandtab shiftwidth=2 softtabstop=2 tabstop=2

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
"au FileType html imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

"----------------------------------------------
" Language: CSS
"----------------------------------------------
au FileType css set expandtab shiftwidth=2 softtabstop=2 tabstop=2
au FileType css nmap <silent> <Leader>; <Plug>(cosco-commaOrSemiColon)
au FileType css imap <silent> <Leader>; <c-o><Plug>(cosco-commaOrSemiColon)

"----------------------------------------------
" Language: JavaScript
"----------------------------------------------
au FileType javascript.* set expandtab shiftwidth=2 softtabstop=2 tabstop=2
au FileType javascript.* nmap <silent> <Leader>; <Plug>(cosco-commaOrSemiColon)
au FileType javascript.* imap <silent> <Leader>; <c-o><Plug>(cosco-commaOrSemiColon)
"au FileType javascript.jsx imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

"----------------------------------------------
" Language: JSON
"----------------------------------------------
au FileType json set expandtab shiftwidth=2 softtabstop=2 tabstop=2

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

"----------------------------------------------
" Language: Ruby
"----------------------------------------------
au FileType ruby set expandtab shiftwidth=2 softtabstop=2 tabstop=2

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
