local present1, autopairs = pcall(require, "nvim-autopairs")
local present2, cmp = pcall(require, "cmp")

if not present1 and present2 then
   return
end

autopairs.setup {
   fast_wrap = {},
   disable_filetype = { "TelescopePrompt", "vim" },
}
autopairs.add_rules(require "nvim-autopairs.rules.endwise-ruby")

local cmp_autopairs = require "nvim-autopairs.completion.cmp"

cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
