local beautiful = require("beautiful")
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local notify = require("naughty").notify
local cairo = require("lgi").cairo
local transform = gears.shape.transform
local icon = {}

local function cr_image_create()
	return cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
end

local function template(icon, callback)
	local widget = wibox.widget {
		widget = wibox.container.place,
		{
			widget = wibox.container.background,
			shape = gears.shape.rounded_rect,
			{
				widget = wibox.container.margin,
				margins = 5,
				{
					widget = wibox.widget.imagebox,
					forced_height = 20,
					forced_width = 20,
					image = icon
				}
			}
		}
	}

	widget.widget:connect_signal("mouse::leave", function(self) self.bg = nil end)
	widget.widget:connect_signal(
		"mouse::enter",
		function(self) self.bg = "#00000030" end
	)
	widget.widget:connect_signal(
		"button::press",
		function(_, _, _, button) if button == 1 then callback() end end
	)
	return widget
end

function icon.close(callback)
	local icon = cr_image_create()
	local cr = cairo.Context(icon)
	cr:set_source(gears.color(beautiful.fg_normal))
	transform(gears.shape.cross)
		:translate(10, 0)
		:rotate(math.pi / 4)(cr, 15, 15, 2)
	cr:fill()
	return template(icon, callback)
end

function icon.maximize(callback)
	local icon = cr_image_create()
	local cr = cairo.Context(icon)
	local w = 15
	cr:set_source(gears.color(beautiful.fg_normal))
	transform(gears.shape.rectangle):translate(2, 2)(cr, w, 2)
	transform(gears.shape.rectangle):translate(2, 2)(cr, 2, w)
	transform(gears.shape.rectangle):translate(w, 4)(cr, 2, w - 2)
	transform(gears.shape.rectangle):translate(2, w)(cr, w, 2)
	cr:fill()
	return template(icon, callback)
end

function icon.minimize(callback)
	local icon = cr_image_create()
	local cr = cairo.Context(icon)
	local w = 15
	cr:set_source(gears.color(beautiful.fg_normal))
	transform(gears.shape.rectangle):translate(2, 8)(cr, w, 2)
	cr:fill()
	return template(icon, callback)
end

return icon
