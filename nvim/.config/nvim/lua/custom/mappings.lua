local map = nvchad.map

-- telescope
map("n", "<leader>fp", ":Telescope media_files<CR>")
map("n", "<leader>te", ":Telescope<CR>")
map("n", "<leader>'", ":Telescope resume<CR>")
map("n", "<leader>*", ":Telescope grep_string<CR>")
map(
   "v",
   "<leader>*",
   'y:lua require("telescope.builtin").grep_string({ default_text = "<C-r>=escape(@",\'/\\\')<CR>" })<CR>'
)

-- truezen
map("n", "<leader>ta", ":TZAtaraxis<CR>")
map("n", "<leader>tm", ":TZMinimalist<CR>")
map("n", "<leader>tf", ":TZFocus<CR>")

-- Test
map("n", "<leader>tf", ":TestFile<CR>")
map("n", "<leader>tl", ":TestLast<CR>")
map("n", "<leader>tn", ":TestNearest<CR>")
map("n", "<leader>ts", ":TestSuite<CR>")
map("n", "<leader>tv", ":TestVisit<CR>")

-- Projectionist
map("n", "<leader>a", ":A<CR>")

-- LazyGit
map("n", "<leader>go", ":LazyGit<CR>")

-- Terminal
vim.api.nvim_del_keymap("t", "jk")
