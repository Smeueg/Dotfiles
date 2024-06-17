--------------------------------------------------------------------------------
--- A widget to show the current layout for the Wibar
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local icon = require("ui.icons")
local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")

local capi = {
	root = root,
	tag = tag
}

local function switch_to_next_layout()
	local layouts = awful.layout.suit
	local current_layout = capi.root.tags()[1].layout
	local next_layout
	if current_layout == layouts.tile.right then
		next_layout = layouts.max
	elseif current_layout == layouts.max then
		next_layout = layouts.floating
	else
		next_layout = layouts.tile.right
	end
	awful.layout.set_all(next_layout)
end

local widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.wibar_widget_shape,
	bg = "#00000030",
	buttons = awful.button({}, 1, switch_to_next_layout),
	{ widget = wibox.widget.imagebox }
}

--- Updates the Widget's imagebox
---@param t tag
local function update(t)
	if t.index ~= 1 then return end
	widget.widget.image = nil
	if t.layout == awful.layout.suit.tile.right then
		widget.widget.image = icon.tile
	elseif t.layout == awful.layout.suit.max then
		widget.widget.image = icon.max
	else
		widget.widget.image = icon.float
	end
end

capi.tag.connect_signal("property::layout", update)

return widget
