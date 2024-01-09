--------------------------------------------------------------------------------
--- Utilities for popups
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local wibox = require("wibox")
local beautiful = require("beautiful")
local enum = require("lib.enum")
local utils = {}

---@class Orientation
local Orientation = enum {"Horizontal", "Vertical"}

--- Creates a singular border
---@param orientation Orientation
local function border(orientation)
	local attr
	if orientation == Orientation.Horizontal then
		attr = "height"
	elseif orientation == Orientation.Vertical then
		attr = "width"
	else
		error("border(): Requires argument of type 'Orientation'")
	end

	return {
		widget = wibox.container.background,
		bg = beautiful.border_focus,
		{
			widget = wibox.container.constraint,
			["forced_" .. attr] = beautiful.border_width,
		}
	}
end


--- Creates borders for popups, similar to client borders
---@param widget wibox.widget|table The widget to wrap
---@param positions table A table of positions of where the borders should show
---                up { "top", "bottom", "left", "right" }
---@return table The wrapped widget
function utils.border_wrapper(widget, positions)
	local borders = {}
	for _, position in ipairs(positions) do
		local orientation
		if position == "top" or position == "bottom" then
			orientation = Orientation.Horizontal
		elseif position == "left" or position == "right" then
			orientation = Orientation.Vertical
		else
			error(string.format(
					"border_wrapper(): %s is not a valid position",
					position
			))
		end
		borders[position] = border(orientation)
	end

	for _, position in ipairs({"top", "bottom", "left", "right"}) do
		if borders[position] == nil then
			borders[position] = { widget = wibox.container.background }
		end
	end
	positions = positions or {}


	return {
		widget = wibox.container.background,
		{
			layout = wibox.layout.align.vertical,
			borders["top"],
			{
				layout = wibox.layout.align.horizontal,
				borders["left"],
				widget,
				borders["right"],
			},
			borders["bottom"]
		}
	}
end

return utils
