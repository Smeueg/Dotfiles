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

-- Load a pulseaudio droidcam module if 'droidcam-cli' is running
local function load_droidcam_module()
	local module_name = "DroidcamAudio"
	local functions
	functions = {
		{
			"ps -C droidcam-cli --no-header -o 'cmd'",
			function(stdout)
				if stdout:match("-a") then
					awful.spawn.easy_async(functions[2][1], functions[2][2])
				end
			end,
		},
		{
			"pactl list short",
			function(stdout)
				if not stdout:match(module_name) then
					awful.spawn.easy_async(functions[3][1], functions[3][2])
				end
			end,
		},
		{
			"pactl load-module module-alsa-source device=hw:Loopback,1,0 source_properties=device.description="
				.. module_name,
			function(stdout)
				awful.spawn.easy_async(functions[4][1], functions[4][2])
			end,
		},
		{
			"pactl list short",
			function(stdout)
				if stdout:match(module_name) then
					naughty.notify({ text = "Successfully loaded Droidcam module" })
					awful.spawn.with_shell(
						"pactl set-default-source 'alsa_input.hw_Loopback_1_0' && pactl set-source-volume @DEFAULT_SINK@ 150%"
					)
				end
			end,
		},
	}

	awful.spawn.easy_async_with_shell(
		"ps -C droidcam-cli && ps -C pulseaudio",
		function(stdout, stderr, reason, exit_code)
			if exit_code == 0 then
				awful.spawn.easy_async(functions[1][1], functions[1][2])
			end
		end
	)
end


-- Icons
local icon_volume = cairo_create(cairo_format, 20, 20)
local icon_mute = cairo_create(cairo_format, 20, 20)
do
	local cr
	local icon_color = gears.color(beautiful.menubar_bg_focus)
	local pi = math.pi

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
end


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
		function() module.timer:emit_signal("timeout") end
	)
end

module.timer = gears.timer {
	timeout = 5,
	callback = function()
		awful.spawn.easy_async(
			"pactl info",
			function(stdout)
				local sink = stdout:match("Default Sink: ([^\n]+)")
				sink = sink:gsub("-", "[-]")
				awful.spawn.easy_async(
					"pactl list sinks",
					function(stdout)
						local regex, str, vol, icon
						regex = ""
						for i=1, 9 do regex = regex .. "[^\n]*\n" end
						str = stdout:match("Name: " .. sink .. regex)
						vol = str:match("(%d+%%)") .. " "
						icon = str:find("Mute: no") and icon_volume or icon_mute
						module.widget:get_children_by_id("icon")[1].image = icon
						module.widget:get_children_by_id("text")[1].text = vol
					end
				)
			end
		)
	end
}

module:ctrl("default")

return module
