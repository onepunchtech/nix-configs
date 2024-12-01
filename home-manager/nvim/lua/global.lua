vim.g.mapleader = ' '

local opt = vim.opt 

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
local startify = require("alpha.themes.startify")
require('alpha').setup(startify.config)
require('which-key').setup()
require('dressing').setup()
