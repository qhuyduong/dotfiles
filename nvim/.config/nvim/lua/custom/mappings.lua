local M = {}

M.disabled = {
  t = {
    ["jk"] = "",
  },
  n = {
    ["<C-h>"] = "",
    ["<C-j>"] = "",
    ["<C-k>"] = "",
    ["<C-l>"] = "",
  },
}

M.telescope = {
  n = {
    ["<leader>/"] = { "<cmd> Telescope live_grep <CR>", "live grep" },
    ["<leader>'"] = { "<cmd> Telescope resume <CR>", "resume" },
    ["<leader>*"] = { "<cmd> Telescope grep_string <CR>", "grep string" },
    ["<leader>d/"] = { '<cmd> Telescope live_grep search_dirs={"%:p:h"} <CR>', "live grep in folder" },
    ["<leader>d*"] = { '<cmd> Telescope live_grep search_dirs={"%:p:h"} <CR>', "grep string in folder" },
    ["<leader>fr"] = { "<cmd> Telescope oldfiles only_cwd=true<CR>", "   find oldfiles" },
  },

  v = {
    ["<leader>*"] = {
      'y:lua require("telescope.builtin").grep_string({ default_text = "<C-r>=escape(@",\'/\\\')<CR>" })<CR>',
      "grep string",
    },
  },
}

M.projectionist = {
  n = {
    ["<leader>a"] = { "<cmd> A <CR>", "alternate" },
  },
}

M.test = {
  n = {
    ["<leader>tf"] = { "<cmd> TestFile <CR>", "test file" },
    ["<leader>tl"] = { "<cmd> TestLast <CR>", "test last" },
    ["<leader>tn"] = { "<cmd> TestNearest <CR>", "test nearest" },
    ["<leader>ts"] = { "<cmd> TestSuite <CR>", "test suite" },
    ["<leader>tv"] = { "<cmd> TestVisit <CR>", "test visit" },
  },
}

M.lazygit = {
  n = {
    ["<leader>go"] = { "<cmd> LazyGit <CR>", "lazygit" },
  },
}

M.windows = {
  n = {
    ["<leader>ws"] = { "<cmd> split <CR>", "split" },
    ["<leader>wv"] = { "<cmd> vsplit <CR>", "vsplit" },
    ["<leader>wd"] = { "<C-W>c", "" },
  },
}

M.general = {
  n = {
    ["<leader>fy"] = { ':let @+=expand("%")<CR>', "copy file path" },
  },
  i = {
    ["jk"] = { "<ESC>", "escape insert mode", opts = { nowait = true } },
  },
}

M.fugitive = {
  n = {
    ["<leader>gl"] = { "<cmd> Git log -10 -- % <CR>", "" },
    ["<leader>gb"] = { "<cmd> Git blame <CR>", "" },
    ["<leader>gt"] = { "<cmd> %Gclog <CR>", "" },
  },
}

M.ranger = {
  n = {
    ["<leader>or"] = { "<cmd> Ranger <CR>", "" },
  },
}

M.hop = {
  n = {
    ["gj"] = { "<cmd> HopLineAC <CR>", "" },
    ["gk"] = { "<cmd> HopLineBC <CR>", "" },
  },

  v = {
    ["gj"] = { "<cmd> HopLineAC <CR>", "" },
    ["gk"] = { "<cmd> HopLineBC <CR>", "" },
  },
}

return M
