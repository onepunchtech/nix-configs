vim.g.mapleader = ' '

local opt = vim.opt 

opt.number = true
opt.number = true
opt.tabstop = 2
opt.shiftwidth = 2
opt.expandtab = true
opt.autoindent = true
opt.ignorecase = true
opt.smartcase = true

opt.backspace = "indent,eol,start"

opt.clipboard:append("unnamedplus")

require('lualine').setup()
