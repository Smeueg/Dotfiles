local upower    = require("daemons.upower")
local beautiful = require("beautiful")
local wibox     = require("wibox")
local naughty   = require("naughty")
local awful     = require("awful")
local icon      = require("ui.icons")


local widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.shape_universal,
	bg = "#00000030",
	buttons = awful.button({}, 1, upower.show_time),
	{
		widget = wibox.container.background,
		layout = wibox.layout.fixed.horizontal,
		{
			widget = wibox.widget.imagebox,
			image = icon.battery_none,
			id = "icon"
		},
		{
			widget = wibox.widget.textbox,
			text = "-",
			id = "percentage"
		},
		{
			widget = wibox.widget.textbox,
			text = "% "
		}
	}
}

local widget_icon       = widget:get_children_by_id("icon")[1]
local widget_percentage = widget:get_children_by_id("percentage")[1]

local function set_icon(state)
	widget.state = state
	if state == upower.state.CHARGING then
		widget_icon.image = icon.battery_charging
		if widget.notification then
			naughty.destroy(widget.notification, nil)
			widget.notification = nil
		end
	elseif state == upower.state.DISCHARGING then
		widget_icon.image = icon.battery_discharging
	end
end

local function set_percentage(percentage)
	widget.percentage = math.floor(percentage)
	widget_percentage.text = widget.percentage
	if widget.percentage <= 15 and widget.state == upower.state.DISCHARGING then
		widget.notification = naughty.notify {
			title = "Warning",
			text = "Your battery is at " .. widget.percentage .. "%",
			timeout = 0,
			replaces_id = 0
		}
	end
end

local function watch_handler(property)
	if property.name == "State" then
		set_icon(property.value)
	elseif property.name == "Percentage" then
		set_percentage(property.value)
	end
end

local info = upower.get_bat_info()
if info then
	set_icon(info.state)
	set_percentage(info.percentage)
end

upower.watch_battery(watch_handler)

return widget
