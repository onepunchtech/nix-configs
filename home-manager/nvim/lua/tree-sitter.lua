require('ibl').setup {
  scope = { enabled = true },
}

require('nvim-ts-autotag').setup()
require('nvim-treesitter.configs').setup {
  -- ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline" },

  sync_install = false,

  auto_install = false,

  ignore_install = { "all" },

  highlight = {
    enable = true,

    additional_vim_regex_highlighting = false,
  },

  indent = { enable = true },

  autotag = {
    enable = true
  }
}
