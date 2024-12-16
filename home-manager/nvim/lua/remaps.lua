local km = vim.keymap

km.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
km.set("n", "<leader>nh", ":nohl<CR>", {})
km.set("n", "<c-l>", "<c-w>l", { desc = "move to window on the right" })
km.set("n", "<c-h>", "<c-w>h", { desc = "move to window on the left" })
km.set("n", "<c-k>", "<c-w>k", { desc = "move to window above" })
km.set("n", "<c-j>", "<c-w>j", { desc = "move to window below" })
