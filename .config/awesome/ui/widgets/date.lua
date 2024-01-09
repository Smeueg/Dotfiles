--------------------------------------------------------------------------------
--- A date widget for the Wibar
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local wibox = require("wibox")
local beautiful = require("beautiful")

local widget = {
	widget = wibox.container.background,
	shape = beautiful.wibar_widget_shape,
	bg = "#00000030",
	{
		widget = wibox.widget.textclock,
		format = " %a  %d·%m·%y  %H:%M "
	}
}

return widget
