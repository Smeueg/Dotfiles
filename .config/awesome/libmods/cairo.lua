--------------------------------------------------------------------------------
--- Extra functions for cairo
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local cairo = require("lgi").cairo

--- Create an image, evaluating body
---@param body any
---@param size number[]
---@return fun(cr: cairo.Context)
function cairo.CreateImage(body, size)
	local tmp = {}

	if not size then -- default value for `size`
		size = { 20, 20 }
	end

	tmp.image = cairo.ImageSurface.create(
		cairo.Format.ARGB32,
		size[1],
		size[2]
	)

	body(cairo.Context(tmp.image))
	return tmp.image
end

--- Convert a cairo surface to rgba
---@param surface cairo.ImageSurface
---@return string
function cairo.surface_to_rgba(surface)
	local _, r, g, b = surface:get_rgba()
	r = math.floor(r * 255 + 0.5)
	g = math.floor(g * 255 + 0.5)
	b = math.floor(b * 255 + 0.5)
	return string.format("#%02X%02X%02X", r, g, b)
end
