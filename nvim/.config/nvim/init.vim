set encoding=utf-8
scriptencoding utf-8

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')
" Make sure you use single quotes
" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
" Junegunn Choi's plugins
Plug 'junegunn/fzf.vim'                   " Fuzzy Finder
Plug 'junegunn/vim-easy-align'            " Easily alignment
Plug 'junegunn/vim-github-dashboard'      " Browse GitHub events in Vim

" Tim Pope's plugins
Plug 'tpope/vim-bundler'                  " Bundle support in vim
Plug 'tpope/vim-commentary'               " Comment stuff out
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

" UI
Plug 'drewtempelmeyer/palenight.vim'
Plug 'itchyny/lightline.vim'
Plug 'ryanoasis/vim-devicons'             " Add file type glyphs/icons to popular Vim plugins
Plug 'airblade/vim-gitgutter'             " Shows a git diff in the gutter and stages/undoes hunks

" Editing
Plug 'jiangmiao/auto-pairs'               " Auto insert or delete brackets, parens, quotes
Plug 'coderifous/textobj-word-column.vim' " Adds text-objects for word-based columns in Vim
Plug 'ntpeters/vim-better-whitespace'     " Remove trailing whitespaces
Plug 'alvan/vim-closetag'                 " Auto close (X)HTML tags
Plug 'easymotion/vim-easymotion'          " Enhanced vim motions
Plug 'tommcdo/vim-exchange'               " Easy text exchange operator for Vim
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
Plug 'mattn/emmet-vim'                    " Provides support for expanding abbreviations similar to emmet
Plug 'pangloss/vim-javascript'            " Vastly improved Javascript indentation and syntax support in Vim
Plug 'mxw/vim-jsx'                        " React JSX syntax highlighting and indenting for vim

" Language: Json
Plug 'elzr/vim-json'                      " A better JSON for Vim

" Language: Proto
Plug 'qhuyduong/vim-proto'                " Google Proto Buffer syntax highlighting

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neoclide/coc-denite'

" Initialize plugin system
call plug#end()

"----------------------------------------------
" General settings
"----------------------------------------------
filetype plugin indent on
syntax enable " Allow vim to set a custom font or color for a word
set number                        " show number ruler
set relativenumber                " show relative numbers in the ruler
set expandtab softtabstop=2 tabstop=2 shiftwidth=2 " global indentation
set title                         " let vim set the terminal title
set updatetime=300                " redraw the status bar often
set backspace=indent,eol,start
set undolevels=100
set ignorecase
set smartcase
set inccommand=split          " enables interactive search and replace
set splitbelow " create horizontal splits below the current window
set splitright " create vertical splits to the right of current window
set termguicolors
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

" Set the leader button
let g:mapleader = ','

"----------------------------------------------
" UI
"----------------------------------------------
set background=dark
colorscheme palenight

let g:lightline = {
      \ 'colorscheme': 'palenight',
      \ }

"----------------------------------------------
" Keys Mapping
"----------------------------------------------
" Clear search highlights
nnoremap <Esc> :noh<CR><Esc>

" These mappings will make it so that going to the next one in a search will
" center on the line it's faugroup Utilities
  autocmd!
  " Autosave buffers before leaving them
  autocmd BufLeave * silent! :wa
  " Remove trailing white spaces on save
  autocmd BufWritePre * StripWhitespace
augroup END

augroup Languages
  autocmd FileType apiblueprint setlocal expandtab shiftwidth=4 softtabstop=4 tabstop=4
  autocmd FileType gitcommit setlocal spell textwidth=80
  autocmd FileType gitconfig setlocal noexpandtab shiftwidth=4 softtabstop=4 tabstop=4
  autocmd FileType javascript.jsx imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
augroup END

"----------------------------------------------

"----------------------------------------------
" Plug 'neoclide/coc.nvim'
"----------------------------------------------
" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

"----------------------------------------------
" Plug 'christoomey/vim-tmux-navigator'
"----------------------------------------------
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
" Plug 'janko-m/vim-test'
"----------------------------------------------
let g:test#strategy = 'vimux'
let g:test#preserve_screen = 1

nnoremap <Leader>tn :TestNearest<CR>
nnoremap <Leader>tf :TestFile<CR>
nnoremap <Leader>ts :TestSuite<CR>
nnoremap <Leader>tl :TestLast<CR>
nnoremap <Leader>tv :TestVisit<CR>

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
" Plug 'mhinz/vim-startify'
"----------------------------------------------
let g:startify_change_to_vcs_root = 1

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
" Easy expansion of the active file directory
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
