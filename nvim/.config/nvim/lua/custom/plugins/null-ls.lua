local null_ls = require "null-ls"
local b = null_ls.builtins

local sources = {
   -- Formatting
   b.formatting.prettier,
   b.formatting.rubocop,
   b.formatting.stylua,

   -- Diagnostics
   b.diagnostics.eslint,
   b.diagnostics.luacheck.with { extra_args = { "--global vim" } },
}

local M = {}

M.setup = function()
   null_ls.setup {
      debug = true,
      sources = sources,
      -- format on save
      on_attach = function(client)
         if client.resolved_capabilities.document_formatting then
            vim.cmd "autocmd BufWritePre <buffer> lua vim.lsp.buf.format { async = true }"
         end
      end,
   }
end

return M
