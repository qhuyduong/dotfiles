local null_ls = require "null-ls"
local b = null_ls.builtins

local sources = {

   -- webdev stuff
   b.formatting.deno_fmt,
   b.formatting.prettier,

   -- Lua
   b.formatting.stylua,

   -- Shell
   b.formatting.shfmt,
   b.diagnostics.shellcheck.with { diagnostics_format = "#{m} [#{c}]" },

   -- cpp
   b.formatting.clang_format,

   -- Ruby
   b.formatting.rubocop,
}

local M = {}

M.setup = function()
   null_ls.setup {
      debug = true,
      sources = sources,
      -- format on save
      on_attach = function(client)
         if client.resolved_capabilities.document_formatting then
            vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.format()]]
         end
      end,
   }
end

return M
