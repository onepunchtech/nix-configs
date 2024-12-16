require("oil").setup({
	default_file_explorer = true,
	delete_to_trash = true,
	skip_confirm_for_simple_edits = true,
	view_options = {
		show_hidden = true,
		natural_order = true,
		is_always_hidden = function(name, _)
			return name == ".." or name == ".git"
		end,
	},
	float = {
		padding = 2,
		max_width = 90,
		max_height = 0,
	},
	win_options = {
		wrap = true,
		winblend = 0,
	},
	keymaps = {
		["<C-c>"] = false,
		["q"] = "actions.close",
		["<C-o>s"] = { "actions.select", opts = { vertical = true }, desc = "Open the entry in a vertical split" },
		["<C-o>h"] = { "actions.select", opts = { horizontal = true }, desc = "Open the entry in a horizontal split" },
		["<C-o>r"] = "actions.refresh",
		["g?"] = { "actions.show_help", mode = "n" },
		["<CR>"] = "actions.select",
		["<C-t>"] = { "actions.select", opts = { tab = true } },
		["<C-p>"] = "actions.preview",
		["-"] = { "actions.parent", mode = "n" },
		["_"] = { "actions.open_cwd", mode = "n" },
		["`"] = { "actions.cd", mode = "n" },
		["~"] = { "actions.cd", opts = { scope = "tab" }, mode = "n" },
		["gs"] = { "actions.change_sort", mode = "n" },
		["gx"] = "actions.open_external",
		["g."] = { "actions.toggle_hidden", mode = "n" },
		["g\\"] = { "actions.toggle_trash", mode = "n" },
	},
	use_default_keymaps = false,
})
