set nocompatible
filetype off

set term=xterm
set t_Co=256
let &t_AB="\e[48;5;%dm"
let &t_AF="\e[38;5;%dm"

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle
" required!
Plugin 'gmarik/Vundle.vim'

" My Plugins here:
"
" original repos on github
Plugin 'tpope/vim-fugitive'
" Plugin 'Lokaltog/vim-easymotion'
" Plugin 'rstacruz/sparkup', {'rtp': 'vim'}
" Plugin 'tpope/vim-rails.git'
" Plugin 'ack.vim'
Plugin 'sjl/badwolf'
Plugin 'plasticboy/vim-markdown'
Plugin 'jtratner/vim-flavored-markdown'
Plugin 'groenewege/vim-less'
Plugin 'editorconfig-vim'
Plugin 'bling/vim-airline'
" Plugin 'airblade/vim-gitgutter'
Plugin 'kien/ctrlp.vim'
" vim-scripts repos
" Plugin 'L9'
" Plugin 'FuzzyFinder'
Plugin 'scrooloose/NERDTree'
Plugin 'scrooloose/NERDCommenter'
" Plugin 'scrooloose/syntastic'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'Tabular'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'leafgarland/typescript-vim'
" Plugin 'Valloric/YouCompleteMe'
Plugin 'ternjs/tern_for_vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'bronson/vim-trailing-whitespace'
" non github repos
" Plugin 'git://git.wincent.com/command-t.git'
"Plugin 'JCLiang/vim-cscope-utils'
" ...
call vundle#end()            " required
filetype plugin indent on     " required!

Bundle 'majutsushi/tagbar'

" Use mouse in vim
"set mouse=a

set guifont       = "Menlo:12"
let g:colors_name = "badwolf"

set modelines=0
syntax enable
set nonumber
set ruler

" Command T settings
let g:CommandTInputDebounce = 200
let g:CommandTFileScanner = "watchman"
let g:CommandTWildIgnore = &wildignore . ",**/bower_components/*" . ",**/node_modules/*" . ",**/vendor/*"
let g:CommandTMaxHeight = 30
let g:CommandTMaxFiles = 500000

" CtrlP settings
"
let g:ctrlp_map = '<leader>t'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard']  " Windows

" Syntastic
let g:syntastic_javascript_checkers = ['']

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

"Some tips from http://stevelosh.com/blog/2010/09/coming-home-to-vim/"

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

" windows conemu fix
inoremap <Char-0x07F> <BS>
nnoremap <Char-0x07F> <BS>

let mapleader = ","

"Custom settings
"set nofoldenable    " disable folding
let g:vim_markdown_folding_disabled=1
" Enable folding
set foldmethod=indent
set foldlevel=99

" search remap
nnoremap / /\v
vnoremap / /\v
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
" clear search
nnoremap <leader><space> :noh<cr>

" match the next brace
nnoremap <tab> %
vnoremap <tab> %
set wrap
" set textwidth=80
set formatoptions=cqt
set linebreak

" remap movement to move by column layout
nnoremap j gj
nnoremap k gk

"User customizations"

" Strips whitespace
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Select pasted text
nnoremap <leader>v V`]

"Window splitting remap"
nnoremap <leader>w <C-w>v<C-w>l
nnoremap <leader>q <C-w>s<C-w>j
nnoremap <C-h> <C-w>h
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-j> <C-w>j

" Buffers
nnoremap <leader>T :enew<cr>
nnoremap gy :bnext<CR>
nnoremap gt :bprevious<cr>
nnoremap gd :bdelete<cr>
nnoremap <leader>bl :ls<CR>

" Theme stuff
nnoremap <leader>1 :colorscheme obsidian<cr>
nnoremap <leader>2 :colorscheme tomorrow-night-bright<cr>
nnoremap <leader>3 :colorscheme molokai<cr>
nnoremap <leader>4 :colorscheme badwolf<cr>

" badwolf settings
let g:badwolf_darkgutter = 1
let g:badwolf_tabline = 2
let g:badwolf_css_props_highlight = 1
let g:badwolf_html_link_underline = 1

" Airline settings
let g:airline#extensions#tabline#enabled =1
let g:airline_powerline_fonts=1
nnoremap <leader>d :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeFind<CR>

" ----- jistr/vim-nerdtree-tabs -----
" Open/close NERDTree Tabs with \t
"nmap <silent> <leader>t :NERDTreeTabsToggle<CR>
" To have NERDTree always open on startup
"let g:nerdtree_tabs_open_on_console_startup = 0

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
	autocmd BufRead,BufNewFile *.js set expandtab tabstop=4 softtabstop=4 shiftwidth=4 ft=javascript syntax=javascript
	autocmd BufRead,BufNewFile *.ts set ft=typescript syntax=typescript
	autocmd BufRead,BufNewFile *.es6 set ft=javascript syntax=javascript
	autocmd BufRead,BufNewFile *.json set ft=json syntax=javascript
	autocmd BufRead,BufNewFile *.twig set ft=htmldjango
	autocmd BufRead,BufNewFile *.rabl set ft=ruby
	autocmd BufRead,BufNewFile *.jade set ft=jade
	autocmd BufRead,BufNewFile *.py set expandtab tabstop=4 softtabstop=4 shiftwidth=4
	autocmd BufRead,BufNewFile *.html set expandtab tabstop=4 softtabstop=4 shiftwidth=4 ft=html syntax=html
	autocmd BufRead,BufNewFile *.css set expandtab tabstop=4 softtabstop=4 shiftwidth=4 ft=css syntax=css
augroup END

" Whitespace fixes
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

augroup whitespace
	autocmd!
	autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
	autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
	autocmd InsertLeave * match ExtraWhitespace /\s\+$/
	autocmd BufWinLeave * call clearmatches()
augroup END

set undolevels=20
set title

set noerrorbells
set noswapfile
set nobackup
nnoremap ; :

" Tabular
nnoremap <leader>a= :Tabularize /=<CR>
vnoremap <leader>a= :Tabularize /=<CR>
nnoremap <leader>a: :Tabularize /:\zs<CR>
vnoremap <leader>a: :Tabularize /:\zs<CR>

" Custom maps
set pastetoggle=<leader>p
nnoremap <leader>m :w <BAR> !lessc % > %:t:r.css<CR><space>

nnoremap <leader>vi :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>
vnoremap <leader>" <esc>`<i"<esc>`>a"<esc>
nnoremap <leader>re gg=G

" Save
noremap  <silent> <C-S> :update<CR>
vnoremap <silent> <C-S> <C-C>:update<CR>
inoremap <silent> <C-S> <C-O>:update<CR>

" Abbreviations
iabbrev adn and
iabbrev waht what
nnoremap H 00
nnoremap L $
inoremap jk <esc>

set fileformat=unix
set fileformats=unix,dos

" -- solarized personal conf
set background=dark
" Uncomment the next line if your terminal is not configured for solarized
colorscheme solarized


nmap <F8> :TagbarToggle<CR>

" Abbreviations
"augroup abbreviations
"autocmd!
"autocmd FileType html :iabbrev <buffer> --- &mdash;
"autocmd FileType javascript :iabbrev <buffer> ret return
"augroup END

" Disable auto comment
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CSCOPE settings for vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" This file contains some boilerplate settings for vim's cscope interface,
" plus some keyboard mappings that I've found useful.
"
" USAGE:
" -- vim 6:     Stick this file in your ~/.vim/plugin directory (or in a
"               'plugin' directory in some other directory that is in your
"               'runtimepath'.
"
" -- vim 5:     Stick this file somewhere and 'source cscope.vim' it from
"               your ~/.vimrc file (or cut and paste it into your .vimrc).
"
" NOTE:
" These key maps use multiple keystrokes (2 or 3 keys).  If you find that vim
" keeps timing you out before you can complete them, try changing your timeout
" settings, as explained below.
"
" Happy cscoping,
"
" Jason Duell       jduell@alumni.princeton.edu     2002/3/7
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nmap <C-l> :tabnext<CR>
nmap <C-h> :tabprev<CR>
set undolevels=100
set cindent
set smarttab
"set shiftwidth=4
"set tabstop=4
"set ts=4
"for kernel
set tabstop=8 softtabstop=8 shiftwidth=8 noexpandtab
"for tianocore dev
"set tabstop=2 softtabstop=2 shiftwidth=2 expandtab

"autocmd InsertEnter * syn clear EOLWS | syn match EOLWS excludenl /\s\+\%#\@!$/
"autocmd InsertLeave * syn clear EOLWS | syn match EOLWS excludenl /\s\+$/
"highlight EOLWS ctermbg=red guibg=red

" This tests to see if vim was configured with the '--enable-cscope' option
" when it was compiled.  If it wasn't, time to recompile vim...
if has ("cscope")


	""""""""""""" Standard cscope/vim boilerplate

	" use both cscope and ctag for 'ctrl-]', ':ta', and 'vim -t'
	set cscopetag

	" check cscope for definition of a symbol before checking ctags: set to 1
	" if you want the reverse search order.
	set csto=0

	" show msg when any other cscope db added
	set nocscopeverbose


	""""""""""""" My cscope/vim key mappings
	"
	" The following maps all invoke one of the following cscope search types:
	"
	"   's'   symbol: find all references to the token under cursor
	"   'g'   global: find global definition(s) of the token under cursor
	"   'c'   calls:  find all calls to the function name under cursor
	"   't'   text:   find all instances of the text under cursor
	"   'e'   egrep:  egrep search for the word under cursor
	"   'f'   file:   open the filename under cursor
	"   'i'   includes: find files that include the filename under cursor
	"   'd'   called: find functions that function under cursor calls
	"
	" Below are three sets of the maps: one set that just jumps to your
	" search result, one that splits the existing vim window horizontally and
	" diplays your search result in the new window, and one that does the same
	" thing, but does a vertical split instead (vim 6 only).
	"
	" I've used CTRL-\ and CTRL-@ as the starting keys for these maps, as it's
	" unlikely that you need their default mappings (CTRL-\'s default use is
	" as part of CTRL-\ CTRL-N typemap, which basically just does the same
	" thing as hitting 'escape': CTRL-@ doesn't seem to have any default use).
	" If you don't like using 'CTRL-@' or CTRL-\, , you can change some or all
	" of these maps to use other keys.  One likely candidate is 'CTRL-_'
	" (which also maps to CTRL-/, which is easier to type).  By default it is
	" used to switch between Hebrew and English keyboard mode.
	"
	" All of the maps involving the <cfile> macro use '^<cfile>$': this is so
	" that searches over '#include <time.h>" return only references to
	" 'time.h', and not 'sys/time.h', etc. (by default cscope will return all
	" files that contain 'time.h' as part of their name).


	" To do the first type of search, hit 'CTRL-\', followed by one of the
	" cscope search types above (s,g,c,t,e,f,i,d).  The result of your cscope
	" search will be displayed in the current window.  You can use CTRL-T to
	" go back to where you were before the search.
	"
	nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
	nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
	nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
	nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
	nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
	nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
	nmap <C-\>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
	nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>

	" Using 'CTRL-spacebar' (intepreted as CTRL-@ by vim) then a search type
	" makes the vim window split horizontally, with search result displayed in
	" the new window.
	"
	" (Note: earlier versions of vim may not have the :scs command, but it
	" can be simulated roughly via:
	"    nmap <C-@>s <C-W><C-S> :cs find s <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@>s :tab scs find s <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@>g :tab scs find g <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@>c :tab scs find c <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@>t :tab scs find t <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@>e :tab scs find e <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@>f :tab scs find f <C-R>=expand("<cfile>")<CR><CR>
	nmap <C-@>i :tab scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
	nmap <C-@>d :tab scs find d <C-R>=expand("<cword>")<CR><CR>

	" Hitting CTRL-space *twice* before the search type does a vertical
	" split instead of a horizontal one (vim 6 and up only)
	"
	" (Note: you may wish to put a 'set splitright' in your .vimrc
	" if you prefer the new window on the right instead of the left
	nmap <C-@><C-@>s :vert scs find s <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@><C-@>g :vert scs find g <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@><C-@>c :vert scs find c <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@><C-@>t :vert scs find t <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@><C-@>e :vert scs find e <C-R>=expand("<cword>")<CR><CR>
	nmap <C-@><C-@>f :vert scs find f <C-R>=expand("<cfile>")<CR><CR>
	nmap <C-@><C-@>i :vert scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
	nmap <C-@><C-@>d :vert scs find d <C-R>=expand("<cword>")<CR><CR>
	nmap <F5> :!cscope -b -q -k -R<CR>:cs reset<CR><CR>

	""""""""""""" key map timeouts
	"
	" By default Vim will only wait 1 second for each keystroke in a mapping.
	" You may find that too short with the above typemaps.  If so, you should
	" either turn off mapping timeouts via 'notimeout'.
	"
	set notimeout
	"
	" Or, you can keep timeouts, by uncommenting the timeoutlen line below,
	" with your own personal favorite value (in milliseconds):
	"
	set timeoutlen=4000
	"
	" Either way, since mapping timeout settings by default also set the
	" timeouts for multicharacter 'keys codes' (like <F1>), you should also
	" set ttimeout and ttimeoutlen: otherwise, you will experience strange
	" delays as vim waits for a keystroke after you hit ESC (it will be
	" waiting to see if the ESC is actually part of a key code like <F1>).
	"
	set ttimeout
	"
	" personally, I find a tenth of a second to work well for key code
	" timeouts. If you experience problems and have a slow terminal or network
	" connection, set it higher.  If you don't set ttimeoutlen, the value for
	" timeoutlent (default: 1000 = 1 second, which is sluggish) is used.
	"
	set ttimeoutlen=100

	" add any cscope database in current directory
	if filereadable("cscope.out")
		cs add cscope.out
		" else add the database pointed to by environment variable
	elseif $CSCOPE_DB != ""
		cs add $CSCOPE_DB
	endif

endif
