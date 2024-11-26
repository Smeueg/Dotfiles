--------------------------------------------------------------------------------
--- Extra Functions for Beautiful
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local beautiful  = require("beautiful")
local gears = require("gears")
local default_colors = {
	color0 = "#202020",
	color1 = "#AC4142",
	color2 = "#90A959",
	color3 = "#F4BF75",
	color4 = "#6A9FB5",
	color5 = "#AA759F",
	color6 = "#75B5AA",
	color7 = "#D0D0D0",
	-- color0 = "#202020",
	-- color0 = "#222222",
	-- color0 = "#303030",
	color8 = "#404040"
}


---@alias hexcolor string
---@alias color_id string

---@param file string A file path to a theme.ini
---@return table<color_id, hexcolor>
function beautiful.fetch_color(file)
	local colors = {}

	if not gears.filesystem.file_readable(file) then
		return default_colors
	end

	local f = io.open(file, "r")

	if f == nil then return default_colors end

	for line in f:lines() do
		if line:sub(1, 1) ~= "#" then -- Ignore comments
			local key, value = line:match("^(%w+)%s*=%s*(%S+)")
			if value:sub(1, 1) == "$" then
				value = colors[value:match("%$(%S+)")]
			end
			colors[key] = value
		end
	end
	f:close()

	-- Check if all the colors needed are set
	for i = 0, 15, 1 do
		if colors["color"..i] == nil then
			return default_colors
		end
	end

	if colors["accent"] == nil then
		return default_colors
	end

	return colors
end
