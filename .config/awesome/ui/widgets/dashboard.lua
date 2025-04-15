--------------------------------------------------------------------------------
--- A Dashboard Widget For The Wibar
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local icons = require("ui.icons")
local dpi = beautiful.xresources.apply_dpi
local dashboard_popup = require("ui.popup.dashboard")

local widget = wibox.widget {
	widget = wibox.container.background,
	shape = gears.shape.rounded_rect_auto,
	bg = "#00000030",
	buttons = gears.table.join(
		awful.button({}, 1, dashboard_popup.toggle)
	),
	{
		widget = wibox.container.margin,
		margins = dpi(6),
		{
			widget = wibox.widget.imagebox,
			image = icons.dashboard
		}
	}
}

wibox.add_clickable(widget)

return widget
