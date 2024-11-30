local subst = require('substitute')

subst.setup()

vim.keymap.set('n', 's', subst.operator, {desc = 'Substitute with motion'})
vim.keymap.set('n', 'ss', subst.line, {desc = 'Substitute line'})
vim.keymap.set('n', 'S', subst.eol, {desc = 'Substitute to end of line'})
vim.keymap.set('x', 's', subst.eol, {desc = 'Substitute in visual mode'})

