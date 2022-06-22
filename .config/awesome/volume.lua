local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local cairo = require("lgi").cairo
local cairo_context = cairo.Context
local cairo_format = cairo.Format.ARGB32
local cairo_create = cairo.ImageSurface.create
local transform = gears.shape.transform
local module = {}

-- Icons
local cr
local icon_color = gears.color(beautiful.menubar_bg_focus)
local pi = math.pi
local icon_volume = cairo_create(cairo_format, 20, 20)
cr = cairo_context(icon_volume)
cr:set_source(icon_color)
transform(gears.shape.rectangle):translate(3, 8)(cr, 3, 6)
transform(gears.shape.isosceles_triangle)
	:rotate_at(6, 6, pi / -2)
	:translate(-4.5, 2)(cr, 12, 9)
transform(gears.shape.arc)
	:translate(1.8, 4.5)(cr, 12, 12, 2, pi / -6, pi / 6, true, true)
transform(gears.shape.arc)
	:translate(2, 3)(cr, 15, 15, 2, pi / -4, pi / 4, true, true)
transform(gears.shape.arc)
	:translate(2.2, 1.5)(cr, 18, 18, 2, pi / -3.5, pi / 3.5, true, true)
cr:fill()

local icon_mute = cairo_create(cairo_format, 20, 20)
cr = cairo.Context(icon_mute)
cr:set_source(icon_color)
transform(gears.shape.rectangle):translate(3, 8)(cr, 3, 6)
transform(gears.shape.isosceles_triangle)
	:rotate_at(6, 6, pi / -2)
	:translate(-4.5, 2)(cr, 12, 9)
transform(gears.shape.cross)
	:rotate_at(4.5, 4.5, pi / 4)
	:translate(13, -4)(cr, 9, 9, 3)
cr:fill()

-- Widget for the wibar
module.widget = wibox.widget {
	widget = wibox.container.background,
	shape = gears.shape.rounded_rect,
	bg = "#00000030",
	buttons = gears.table.join(
		awful.button({}, 3, function() module:ctrl("toggle") end),
		awful.button({}, 4, function() module:ctrl("+1") end),
		awful.button({}, 5, function() module:ctrl("-1") end)
	),
	{
		layout = wibox.layout.fixed.horizontal,
		{
			widget = wibox.container.margin,
			margins = 5,
			{
				widget = wibox.widget.imagebox,
				id = "icon",
				image = icon_volume
			},
		},
		{
			widget = wibox.widget.textbox,
			id = "text",
		}
	}
}

local function widget_update()
	awful.spawn.easy_async(
		"pactl list sinks",
		function(stdout)
			local vol = stdout:match("(%d+%%)") .. " "
			local icon
			if stdout:match("Mute: (%w+)") == "yes" then
				icon = icon_mute
			else
				icon = icon_volume
			end
			module.widget:get_children_by_id("icon")[1].image = icon
			module.widget:get_children_by_id("text")[1].text = vol
		end
	)
end

function module:ctrl(cmd)
	local cmds = {
		["+1"] = "set-sink-volume @DEFAULT_SINK@ +1%",
		["-1"] = "set-sink-volume @DEFAULT_SINK@ -1%",
		["+5"] = "set-sink-volume @DEFAULT_SINK@ +5%",
		["-5"] = "set-sink-volume @DEFAULT_SINK@ -5%",
		["toggle"] = "set-sink-mute @DEFAULT_SINK@ toggle",
		["default"] = "set-sink-volume @DEFAULT_SINK@ 35%",
	}

	awful.spawn.easy_async(
		"pactl " .. cmds[cmd],
		widget_update
	)
end

module.timer = gears.timer {
	timeout = 5,
	autostart = true,
	callback = widget_update
}

module:ctrl("default")

return module
