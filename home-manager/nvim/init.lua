vim.o.termguicolors = true

vim.cmd "colorscheme catppuccin"

require("global")
require("tree-sitter")
require("directory")
require("fuzzy-search")
require("remaps")
require("autocomplete")
require("autopairs")
require("comments")
require("substitute")
require("surround")

-- TODO: 
-- auto complete
-- substitute
-- surround
-- lsp
-- gitsigns
-- trouble
-- lazygit
-- org mode
