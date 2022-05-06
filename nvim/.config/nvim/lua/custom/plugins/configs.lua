-- overriding default plugin configs!

local M = {}

M.treesitter = {
   ensure_installed = {
      "vim",
      "html",
      "css",
      "javascript",
      "json",
      "markdown",
      "c",
      "bash",
      "lua",
     "ruby",
   },
}

M.nvimtree = {
   git = {
      enable = true,
   },
}

M.telescope = {
   extensions = {
     -- fd is needed
      media_files = {
         filetypes = { "png", "webp", "jpg", "jpeg" },
      },
   },
}

return M
