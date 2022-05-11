local map = nvchad.map

-- telescope
map("n", "<leader>fp", ":Telescope media_files <CR>")
map("n", "<leader>te", ":Telescope <CR>")
map("n", "<leader>'", ":Telescope resume<CR>")

-- truezen
map("n", "<leader>ta", ":TZAtaraxis <CR>")
map("n", "<leader>tm", ":TZMinimalist <CR>")
map("n", "<leader>tf", ":TZFocus <CR>")

-- Test
map("n", "<leader>tf", "<cmd>TestFile<CR>")
map("n", "<leader>tl", "<cmd>TestLast<CR>")
map("n", "<leader>tn", "<cmd>TestNearest<CR>")
map("n", "<leader>ts", "<cmd>TestSuite<CR>")
map("n", "<leader>tv", "<cmd>TestVisit<CR>")
