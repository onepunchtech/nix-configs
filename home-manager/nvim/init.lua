vim.o.termguicolors = true

vim.cmd "colorscheme catppuccin"

require("global")
require("tree-sitter")
require("directory")
require("fuzzy-search")
require("autocomplete")
require("autopairs")
require("comments")
require("remaps")
require("surround")
require("subst")
require("lsp")
require("formatter")
require("git")

-- TODO: 
-- gitsigns
-- lazygit
-- org mode
--
--
-- substitute
-- line hints scope
-- direnv
-- haskell
-- rust
