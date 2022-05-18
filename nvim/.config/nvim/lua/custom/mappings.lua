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

M.truzen = {
   n = {
      ["<leader>ta"] = { "<cmd> TZAtaraxis <CR>", "   truzen ataraxis" },
      ["<leader>tm"] = { "<cmd> TZMinimalist <CR>", "   truzen minimal" },
      ["<leader>tf"] = { "<cmd> TZFocus <CR>", "   truzen focus" },
   },
}

M.telescope = {
   n = {
      ["<leader>fp"] = { "<cmd> Telescope media_files <CR>", "  find media" },
      ["<leader>/"] = { "<cmd> Telescope live_grep <CR>", "live grep" },
      ["<leader>'"] = { "<cmd> Telescope resume <CR>", "resume" },
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

return M
