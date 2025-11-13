local builtin = require('telescope.builtin')
local km = vim.keymap
km.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
km.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
km.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
km.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })

km.set('n', '<leader>fs', builtin.lsp_document_symbols, {})
km.set('n', '<leader>fi', '<cmd>AdvancedGitSearch<CR>')
km.set('n', '<leader>fw', builtin.grep_string, {})

km.set('n', '<leader>ft', '<cmd>TodoTelescope<cr>', { desc = "Find todos"})


