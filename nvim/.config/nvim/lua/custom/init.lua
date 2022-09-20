local opt = vim.opt
local g = vim.g

-- vim-test
g["test#strategy"] = "vimux"
g["test#preserve_screen"] = 1

-- Projectionist
g.projectionist_heuristics = {
   ["*.rb"] = {
      ["lib/*.rb"] = {
         ["alternate"] = "spec/lib/{}_spec.rb",
         ["type"] = "source",
      },
      ["app/*.rb"] = {
         ["alternate"] = "spec/{}_spec.rb",
         ["type"] = "source",
      },
      ["spec/*_spec.rb"] = {
         ["alternate"] = { "{}.rb", "app/{}.rb" },
         ["type"] = "spec",
      },
   },
   ["*.js"] = {
      ["*.spec.js"] = {
         ["alternate"] = "{dirname}/../{basename}.js",
         ["type"] = "spec",
      },
      ["*.js"] = {
         ["alternate"] = "{dirname}/__tests__/{basename}.spec.js",
         ["type"] = "source",
      },
   },
}
g["loaded_matchit"] = nil

opt.clipboard = ""
opt.relativenumber = true

-- autocmds
vim.cmd [[
iabbrev JIRA https://employmenthero.atlassian.net/browse/<c-o>:call getchar()<CR>
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

augroup Fugitive
  autocmd!
  autocmd BufEnter fugitive://* nnoremap <buffer> <C-n> :cnext<CR> | nnoremap <buffer> <C-p> :cprevious<CR>
augroup END

if has('nvim') && executable('nvr')
  let $GIT_EDITOR = "nvr -cc 'split | only' --remote-wait +'set bufhidden=wipe'"
endif
]]
