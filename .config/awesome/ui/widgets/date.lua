local wibox = require("wibox")
local beautiful = require("beautiful")

local widget_date = {
	widget = wibox.container.background,
	shape = beautiful.shape_universal,
	bg = "#00000030",
	{
		widget = wibox.widget.textclock,
		format = " %a  %d•%m•%y  %H:%M "
	}
}

return function() return widget_date end
