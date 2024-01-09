--------------------------------------------------------------------------------
--- A screenshot popup and widget for the Wibar
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local wibox = require("wibox")
local beautiful = require("beautiful")
local awful = require("awful")
local icons = require("ui.icons")
local popup_screenshot = require("ui.popup.screenshot")
local widget = {
	widget = wibox.container.background,
	shape = beautiful.wibar_widget_shape,
	bg = beautiful.wibar_widget_bg,
	buttons = awful.button(nil, 1, popup_screenshot.toggle),
	{
		widget = wibox.widget.imagebox,
		image = icons.screenshot
	}
}

return widget
