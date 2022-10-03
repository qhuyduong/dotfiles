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
      "lua",
      "ruby",
      "typescript",
   },
}

M.nvimtree = {
   git = {
      enable = true,
   },
}

M.mason = {
   ensure_installed = {
      -- lua stuff
      "lua-language-server",
      "stylua",

      -- web dev
      "css-lsp",
      "html-lsp",
      "typescript-language-server",
      "emmet-ls",
      "json-lsp",
   },
}

return M
