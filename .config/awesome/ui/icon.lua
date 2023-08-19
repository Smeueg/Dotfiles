local cairo = require("lgi").cairo
local gears = require("gears")
local beautiful = require("beautiful")
local shape = gears.shape
local icon_color = gears.color(beautiful.icon_color)
local icon_color2 = gears.color(beautiful.fg_normal)
local pi = math.pi

local icon = {}

function icon_create(body, size)
	local tmp = {}

	if not size then -- default value for `size`
		size = { 20, 20 }
	end

	tmp.image = cairo.ImageSurface.create(cairo.Format.ARGB32, size[1], size[2])

	body(cairo.Context(tmp.image))

	return tmp.image
end

icon.unmuted = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rectangle)
			:translate(6, 8)(cr, 4, 4)
		shape.transform(shape.isosceles_triangle)
			:rotate_at(0, 0, pi / -2)
			:translate(-14, 6)(cr, 8, 4)
		shape.transform(shape.arc)
			:translate(0, 4)(cr, 12, 12, 1, pi / -7, pi / 7, true, true)
		shape.transform(shape.arc)
			:translate(0, 3)(cr, 14, 14, 1, pi / -6, pi / 6, true, true)
		shape.transform(shape.arc)
			:translate(0, 2)(cr, 16, 16, 1, pi / -5, pi / 5, true, true)
		cr:fill()
end)

icon.muted = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rectangle)
			:translate(6, 8)(cr, 4, 4)
		shape.transform(shape.isosceles_triangle)
			:rotate_at(0, 0, pi / -2)
			:translate(-14, 6)(cr, 8, 4)
		shape.transform(shape.cross)
			:rotate_at(0, 0, pi / 4)
			:translate(13.5, -5)(cr, 6, 6, 1)
		cr:fill()
end)

icon.offline = icon_create(function(cr)
		cr:set_source(icon_color)
		local c_start = (1.5 - 1/5) * pi
		local c_end = (1.5 + 1/5) * pi
		shape.transform(shape.pie)
			:translate(0, 5)(cr, 20, 20, c_start, c_end)
		cr:set_line_width(1)
		cr:stroke()
end)

icon.wifi = icon_create(function(cr)
		cr:set_source(icon_color)
		local c_start = (1.5 - 1/5) * pi
		local c_end = (1.5 + 1/5) * pi
		shape.transform(shape.pie)
			:translate(0, 5)(cr, 20, 20, c_start, c_end)
		cr:set_line_width(1)
		cr:fill()
end)

icon.eth = icon_create(function(cr)
		cr:set_source(icon_color)
		cr:rectangle(8, 4, 3, 3)
		cr:rectangle(5, 12, 3, 3)
		cr:rectangle(11, 12, 3, 3)
		cr:rectangle(9, 7, 1, 3)
		cr:rectangle(6, 9, 1, 3)
		cr:rectangle(12, 9, 1, 3)
		cr:rectangle(6, 9, 6, 1)
		cr:fill()
end)

icon.tile = icon_create(function(cr)
		cr:rectangle(4, 4, 5, 12)
		cr:set_source(icon_color)
		cr:fill()
		cr:rectangle(11, 4, 5, 5)
		cr:rectangle(11, 11, 5, 5)
		cr:set_source(icon_color2)
		cr:fill()
end)

icon.max = icon_create(function(cr)
		cr:rectangle(4, 4, 12, 12)
		cr:set_source(icon_color)
		cr:fill()
end)

icon.float = icon_create(function(cr)
		cr:set_source(icon_color)
		cr:rectangle(4, 4, 8, 8)
		cr:fill()
		cr:rectangle(13, 8, 3, 8)
		cr:rectangle(8, 13, 8, 3)
		cr:set_source(icon_color2)
		cr:fill()
end)

icon.shutdown = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.arc)
			:translate(3, 3)(cr, 14, 14, 2, -0.4 * pi, -0.6 * pi)
		shape.transform(shape.rounded_bar)
			:translate(9, 1)(cr, 2, 8)
		cr:fill()
end)

icon.reboot = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.arc)
			:translate(3, 3)(cr, 14, 14, 2, -0.3 * pi, -0.7 * pi)
		shape.transform(shape.isosceles_triangle)
			:translate(15, 2):rotate_at(15, 2, pi / 2.75)(cr, 5, 5)
		cr:fill()
end)

icon.suspend = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.circle):translate(2, 2)(cr, 16, 16)
		cr:fill()
		cr:set_operator(cr, cairo.Operator.clear)
		shape.transform(shape.circle):translate(0, 0)(cr, 12, 12)
		cr:fill()
end)

icon.search = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.circle):translate(4, 4)(cr, 8, 8)
		cr:stroke()
		shape.transform(shape.rectangle)
			:translate(11, 10)
			:rotate_at(11, 10, pi / 4)(cr, 6, 3)
		cr:fill()
end)

icon.screenshot = icon_create(function(cr)
		cr:set_source(icon_color)
		cr:rectangle(4, 4, 4, 2)
		cr:rectangle(4, 4, 2, 4)
		cr:rectangle(4, 14, 4, 2)
		cr:rectangle(4, 12, 2, 4)
		cr:rectangle(12, 4, 4, 2)
		cr:rectangle(14, 4, 2, 4)
		cr:rectangle(12, 14, 4, 2)
		cr:rectangle(14, 12, 2, 4)
		cr:fill()
end)

icon.battery_none = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rounded_rect)
			:translate(15, 10)(cr, 10, 20, 2)
		cr:stroke()
		shape.transform(shape.rectangle)
			:rotate_at(16, 15, pi / 4)
			:translate(16, 15)(cr, 14, 2)
		shape.transform(shape.rounded_rect)
			:translate(17, 6)(cr, 6, 4, 2)
		cr:fill()
end, {40, 40})

icon.battery_charging = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rounded_rect)
			:translate(15, 10)(cr, 10, 20, 2)
		cr:stroke()
		shape.transform(shape.isosceles_triangle)
			:translate(17, 13)(cr, 6, 8)
		shape.transform(shape.isosceles_triangle)
			:rotate_at(23, 27, pi)
			:translate(23, 27)(cr, 6, 8)
		shape.transform(shape.rounded_rect)
			:translate(17, 6)(cr, 6, 4, 2)
		cr:fill()
		cr:set_operator(cr, cairo.Operator.clear)
		shape.transform(shape.rectangle)
			:translate(20, 13)(cr, 3, 6)
		shape.transform(shape.rectangle)
			:translate(17, 20)(cr, 3, 6)
		cr:fill()
end, {40, 40})

icon.battery_discharging = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rounded_rect)
			:translate(15, 10)(cr, 10, 20, 2)
		cr:stroke()
		shape.transform(shape.rounded_rect)
			:translate(17, 6)(cr, 6, 4, 2)
		shape.transform(shape.rounded_rect)
			:translate(18, 17)(cr, 4, 10, 2)
		cr:fill()
end, {40, 40})




icon.dashboard = beautiful.theme_assets.awesome_icon(
	20,
	icon_color2,
	nil
)


return icon
