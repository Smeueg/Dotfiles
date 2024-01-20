--------------------------------------------------------------------------------
--- A screenshot popup and widget for the Wibar
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local popup_screenshot = require("ui.popup.screenshot")
local beautiful        = require("beautiful")
local wibox            = require("wibox")
local awful            = require("awful")
local icons            = require("ui.icons")
local cursor           = require("lib.cursor")

local widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.wibar_widget_shape,
	bg = beautiful.wibar_widget_bg,
	buttons = awful.button(nil, 1, popup_screenshot.toggle),
	{
		widget = wibox.widget.imagebox,
		image = icons.screenshot
	}
}

cursor.add_clickable_to_wibox(widget)

return widget
