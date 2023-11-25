local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local icon = require("ui.icons")
local dpi = beautiful.xresources.apply_dpi

local function vol_toggle()
	awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
end

local function vol_inc()
	awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +1%", false)
end

local function vol_dec()
	awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ -1%", false)
end

local widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.shape_universal,
	bg = "#00000030",
	buttons = gears.table.join(
		awful.button({}, 1, vol_toggle),
		awful.button({}, 4, vol_inc),
		awful.button({}, 5, vol_dec)
	),
	{
		widget = wibox.container.margin,
		layout = wibox.layout.fixed.horizontal,
		left = dpi(5),
		right = dpi(5),
		{
			widget = wibox.container.background,
			{
				widget = wibox.widget.imagebox,
				id = "icon",
			}
		},
		{
			widget = wibox.widget.textbox,
			id = "vol"
		},
	}
}

local widget_vol = widget:get_children_by_id("vol")[1]
local widget_icon = widget:get_children_by_id("icon")[1]

local function parse_volume(stdout)
	widget_vol.text = stdout:match("%d+%%") .. " "
end

local function parse_mute(stdout)
	widget_icon.image = stdout:match("no") and icon.unmuted or icon.muted
end

local function watch()
	awful.spawn.easy_async("pactl get-sink-volume @DEFAULT_SINK@", parse_volume)
	awful.spawn.easy_async("pactl get-sink-mute @DEFAULT_SINK@", parse_mute)
	awful.spawn.with_line_callback(
		"pactl subscribe",
		{
			stdout = function(line)
				if not line:match("'change' on sink ") then return end
				awful.spawn.easy_async(
					"pactl get-sink-volume @DEFAULT_SINK@",
					parse_volume
				)
				awful.spawn.easy_async(
					"pactl get-sink-mute @DEFAULT_SINK@",
					parse_mute
				)
			end,
			exit = watch
		}
	)
end

awful.spawn.with_line_callback(
	"ps -C pactl -o pid=,cmd=",
	{
		stdout = function(line)
			local pid = line:match("(%d+)%s+pactl subscribe")
			if pid then
				awful.spawn("kill -9 " .. pid, false)
			end
		end,
		output_done = watch
	}
)

return widget
