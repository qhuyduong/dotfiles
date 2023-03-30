---@type MappingsTable
local M = {}

M.general = {
  n = {
    [";"] = { ":", "enter command mode", opts = { nowait = true } },
  },
}

M.fugitive = {
  n = {
    ["<leader>gl"] = { "<cmd> Git log -10 -- % <CR>", "" },
    ["<leader>gb"] = { "<cmd> Git blame <CR>", "" },
    ["<leader>gt"] = { "<cmd> %Gclog <CR>", "" },
  },
}

M.windows = {
  n = {
    ["<leader>ws"] = { "<cmd> split <CR>", "split" },
    ["<leader>wv"] = { "<cmd> vsplit <CR>", "vsplit" },
    ["<leader>wd"] = { "<C-W>c", "" },
  },
}

M.lazygit = {
  n = {
    ["<leader>go"] = { "<cmd> LazyGit <CR>", "lazygit" },
  },
}

-- more keybinds!

return M
