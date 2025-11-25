-- Helper: get current visual selection as a string
local function get_visual_selection()
	local start_pos = vim.fn.getpos("'<")
	local end_pos = vim.fn.getpos("'>")

	local start_line, start_col = start_pos[2], start_pos[3]
	local end_line, end_col = end_pos[2], end_pos[3]

	-- ensure correct ordering
	if start_line > end_line or (start_line == end_line and start_col > end_col) then
		start_line, end_line = end_line, start_line
		start_col, end_col = end_col, start_col
	end

	local lines = vim.fn.getline(start_line, end_line)
	if #lines == 0 then
		return ""
	end

	lines[1] = string.sub(lines[1], start_col)
	lines[#lines] = string.sub(lines[#lines], 1, end_col)

	return table.concat(lines, "\n")
end

-- Generic converter function
local function convert_selection(lang)
	local text = get_visual_selection()
	if text == "" then
		vim.notify("No selection", vim.log.levels.WARN)
		return
	end

	local cmd = { "nix-converter", "-l", lang }
	local output = vim.fn.system(cmd, text)

	if vim.v.shell_error ~= 0 then
		vim.notify("nix-converter error: " .. output, vim.log.levels.ERROR)
		return
	end

	-- Set to system clipboard (+)
	vim.fn.setreg("+", output)
	vim.notify("Converted (" .. lang .. ") selection copied to clipboard", vim.log.levels.INFO)
end

-- Two visual-mode mappings:
-- <leader>ny → YAML
-- <leader>nj → JSON
vim.keymap.set("v", "<leader>ny", function()
	convert_selection("yaml")
end, { desc = "Convert YAML selection using nix-converter → clipboard" })

vim.keymap.set("v", "<leader>nj", function()
	convert_selection("json")
end, { desc = "Convert JSON selection using nix-converter → clipboard" })
