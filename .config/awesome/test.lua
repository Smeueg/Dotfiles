local naughty = require("naughty")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local awful = require("awful")
local Gio = require("lgi").Gio
local animate = require("animate")
local pr = function(str)
	naughty.notify({ text = "" .. str })
end


if popup then
	popup.visible = false
	popup = nil
else
	popup = awful.popup {
		placement = awful.placement.centered,
		shape = gears.shape.rounded_rect,
		border_color = beautiful.border_focus,
		border_width = 5,
		ontop = true,
		visible = true,
		bg = beautiful.bg_normal,
		-- bg = beautiful.border_focus,
		widget = {
			widget = wibox.container.margin,
			margins = 50,
		}
	}

	animate.color_transition(
		popup,
		0.25,
		beautiful.bg_normal,
		beautiful.border_focus,
		function(color)
			popup.bg = color
		end
	)
end
