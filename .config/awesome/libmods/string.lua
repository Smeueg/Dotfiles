--------------------------------------------------------------------------------
--- Other useful methods for strings
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
--- Capitalizes the first character of the string
---@return string
function string:capitalize()
	return self:sub(1, 1):upper() .. self:sub(2)
end

--- Creates a markup string used by Pango
--- (see: https://docs.gtk.org/Pango/pango_markup.html)
---@param style table A table of the style
---@return string
function string:to_pango(style)
	local options = ""
	for key, v in pairs(style) do
		options = options .. string.format(" %s='%s'", key, v)
	end
	return string.format("<span%s>%s</span>", options, self)
end
