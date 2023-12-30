local cairo = require("lgi").cairo
local gears = require("gears")
local beautiful = require("beautiful")
local shape = gears.shape
local icon_color = gears.color(beautiful.icon_color)
local icon_color2 = gears.color(beautiful.fg_normal)
local pi = math.pi

local icon = {}

local function icon_create(body, size)
	local tmp = {}

	if not size then -- default value for `size`
		size = { 40, 40 }
	end

	tmp.image = cairo.ImageSurface.create(cairo.Format.ARGB32, size[1], size[2])

	body(cairo.Context(tmp.image))

	return tmp.image
end

icon.unmuted = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rectangle)
			:translate(12, 16)(cr, 8, 8)
		shape.transform(shape.isosceles_triangle)
			:rotate_at(0, 0, pi / -2)
			:translate(-28, 12)(cr, 16, 8)
		shape.transform(shape.arc)
			:translate(0, 8)(cr, 24, 24, 2, pi / -7, pi / 7, true, true)
		shape.transform(shape.arc)
			:translate(0, 6)(cr, 28, 28, 2, pi / -6, pi / 6, true, true)
		shape.transform(shape.arc)
			:translate(0, 4)(cr, 32, 32, 2, pi / -5, pi / 5, true, true)
		cr:fill()
end)

icon.muted = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rectangle)
			:translate(12, 16)(cr, 8, 8)
		shape.transform(shape.isosceles_triangle)
			:rotate_at(0, 0, pi / -2)
			:translate(-28, 12)(cr, 16, 8)
		shape.transform(shape.cross)
			:rotate_at(0, 0, pi / 4)
			:translate(27, -10)(cr, 12, 12, 2)
		cr:fill()
end)

icon.offline = icon_create(function(cr)
		cr:set_source(icon_color)
		local c_start = (1.5 - 1/5) * pi
		local c_end = (1.5 + 1/5) * pi
		shape.transform(shape.pie)
			:translate(0, 10)(cr, 40, 40, c_start, c_end)
		cr:stroke()
end)

icon.wifi = icon_create(function(cr)
		cr:set_source(icon_color)
		local c_start = (1.5 - 1/5) * pi
		local c_end = (1.5 + 1/5) * pi
		shape.transform(shape.pie)
			:translate(0, 10)(cr, 40, 40, c_start, c_end)
		cr:fill()
end)

icon.eth = icon_create(function(cr)
		cr:set_source(icon_color)
		cr:rectangle(16, 8, 6, 6)
		cr:rectangle(10, 24, 6, 6)
		cr:rectangle(22, 24, 6, 6)
		cr:rectangle(18, 14, 2, 6)
		cr:rectangle(12, 18, 2, 6)
		cr:rectangle(24, 18, 2, 6)
		cr:rectangle(12, 18, 12, 2)
		cr:fill()
end)

icon.tile = icon_create(function(cr)
		cr:rectangle(8, 8, 10, 24)
		cr:set_source(icon_color)
		cr:fill()
		cr:rectangle(22, 8, 10, 10)
		cr:rectangle(22, 22, 10, 10)
		cr:set_source(icon_color2)
		cr:fill()
end)

icon.max = icon_create(function(cr)
		cr:rectangle(8, 8, 24, 24)
		cr:set_source(icon_color)
		cr:fill()
end)

icon.float = icon_create(function(cr)
		cr:set_source(icon_color)
		cr:rectangle(8, 8, 16, 16)
		cr:fill()
		cr:rectangle(26, 16, 6, 16)
		cr:rectangle(16, 26, 16, 6)
		cr:set_source(icon_color2)
		cr:fill()
end)

icon.shutdown = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.arc)
			:translate(6, 6)(cr, 28, 28, 4, -0.4 * pi, -0.6 * pi)
		shape.transform(shape.rounded_bar)
			:translate(18, 2)(cr, 4, 16)
		cr:fill()
end)

icon.reboot = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.arc)
			:translate(6, 6)(cr, 28, 28, 4, -0.3 * pi, -0.7 * pi)
		shape.transform(shape.isosceles_triangle)
			:translate(30, 4):rotate_at(30, 4, pi / 2.75)(cr, 10, 10)
		cr:fill()
end)

icon.suspend = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.circle):translate(4, 4)(cr, 32, 32)
		cr:fill()
		cr:set_operator(cr, cairo.Operator.clear)
		shape.transform(shape.circle):translate(0, 0)(cr, 24, 24)
		cr:fill()
end)

icon.search = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.circle):translate(8, 8)(cr, 16, 16)
		cr:stroke()
		shape.transform(shape.rectangle)
			:translate(22, 20)
			:rotate_at(22, 20, pi / 4)(cr, 12, 4)
		cr:fill()
end)

icon.screenshot = icon_create(function(cr)
		cr:set_source(icon_color)
		cr:rectangle(8, 8, 8, 4)
		cr:rectangle(8, 8, 4, 8)
		cr:rectangle(8, 28, 8, 4)
		cr:rectangle(8, 24, 4, 8)
		cr:rectangle(24, 8, 8, 4)
		cr:rectangle(28, 8, 4, 8)
		cr:rectangle(24, 28, 8, 4)
		cr:rectangle(28, 24, 4, 8)
		cr:fill()
end)

--- Batteries
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
end)

icon.battery_charging = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rounded_rect)
			:translate(15, 10)(cr, 10, 20, 2)
		cr:stroke()
		shape.transform(shape.rounded_rect)
			:translate(17, 6)(cr, 6, 4, 2)
		shape.transform(shape.isosceles_triangle)
			:translate(17, 13)(cr, 6, 8)
		shape.transform(shape.isosceles_triangle)
			:rotate_at(23, 27, pi)
			:translate(23, 27)(cr, 6, 8)
		cr:fill()
		cr:set_operator(cr, cairo.Operator.clear)
		shape.transform(shape.rectangle)
			:translate(20, 13)(cr, 3, 6)
		shape.transform(shape.rectangle)
			:translate(17, 20)(cr, 3, 6)
		cr:fill()
end)

icon.battery_discharging = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rounded_rect)
			:translate(15, 10)(cr, 10, 20, 2)
		cr:stroke()
		shape.transform(shape.rounded_rect)
			:translate(17, 6)(cr, 6, 4, 2)
		shape.transform(shape.rounded_rect)
			:translate(18, 18)(cr, 4, 9, 2)
		cr:fill()
end)

icon.battery_fully_charged = icon_create(function(cr)
		cr:set_source(icon_color)
		shape.transform(shape.rounded_rect)
			:translate(15, 10)(cr, 10, 20, 2)
		cr:stroke()
		shape.transform(shape.rounded_rect)
			:translate(17, 6)(cr, 6, 4, 2)
		shape.transform(shape.rounded_rect)
			:translate(18, 12)(cr, 4, 16, 2)
		cr:fill()
end)

icon.dashboard = beautiful.theme_assets.awesome_icon(
	20,
	icon_color2,
	nil
)

function icon.create_battery_discharging(percentage)
	local value = percentage * 16 / 100
	local surface = cairo.ImageSurface.create(
		cairo.Format.ARGB32,
		40,
		40
	)

	local cr = cairo.Context(surface)
	cr:set_source(icon_color)
	shape.transform(shape.rounded_rect)
		:translate(15, 10)(cr, 10, 20, 2)
	cr:stroke()
	shape.transform(shape.rounded_rect)
		:translate(17, 6)(cr, 6, 4, 2)
	shape.transform(shape.rounded_rect)
		:translate(18, 28 - value)(cr, 4, value, 2)
	cr:fill()
	return surface
end

function icon.create_battery_charging(percentage)
	local surface = icon.create_battery_discharging(percentage)
	local cr = cairo.Context(surface)
	cr:set_source(icon_color)
	cr:set_operator(cairo.Operator.XOR)
	cr:move_to(20, 21)
	cr:line_to(18, 21)
	cr:line_to(20.5, 12)
	cr:close_path()
	cr:move_to(20, 19)
	cr:line_to(22, 19)
	cr:line_to(19.5, 28)
	cr:close_path()
	cr:fill()
	return surface
end


return icon
