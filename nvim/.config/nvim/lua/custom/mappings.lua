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

return M
