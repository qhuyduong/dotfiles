-- local autocmd = vim.api.nvim_create_autocmd

-- Auto resize panes when resizing nvim window
-- autocmd("VimResized", {
--   pattern = "*",
--   command = "tabdo wincmd =",
-- })
vim.cmd([[
augroup Fugitive
  autocmd!
  autocmd BufEnter fugitive://* nnoremap <buffer> <C-n> :cnext<CR> | nnoremap <buffer> <C-p> :cprevious<CR>
augroup END
]])
