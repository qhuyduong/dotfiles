set number
set lazyredraw
set scrolloff=10
syntax off

inoremap jj <Esc>
nnoremap <Space> :
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

autocmd BufWritePost * :silent exec "!bsc Reason.re >Javascript.js 2>&1"

hi! Normal ctermfg=black ctermbg=white
