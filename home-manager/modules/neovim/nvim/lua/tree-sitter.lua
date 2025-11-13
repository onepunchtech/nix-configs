require("nvim-ts-autotag").setup()
require("nvim-treesitter.configs").setup({

	sync_install = false,

	auto_install = false,

	ignore_install = { "all" },

	highlight = {
		enable = true,

		additional_vim_regex_highlighting = false,
	},

	indent = { enable = true },
})

require("ibl").setup({
	scope = { enabled = true },
})
