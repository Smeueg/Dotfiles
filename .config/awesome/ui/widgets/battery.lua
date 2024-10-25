--------------------------------------------------------------------------------
--- A battery widget for the Wibar
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local upower         = require("system.upower")
local beautiful      = require("beautiful")
local wibox          = require("wibox")
local awful          = require("awful")
local icons          = require("ui.icons")
local cursor         = require("lib.cursor")

local bat_info       = upower.get_info()

local battery_widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.wibar_widget_shape,
	bg = "#00000030",
	buttons = awful.button({}, 1, function()
			if bat_info then
				bat_info:notify_time()
			end
	end),
	{
		layout = wibox.layout.fixed.horizontal,
		{
			widget = wibox.widget.imagebox,
			image = icons.battery_none,
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

cursor.add_clickable_to_wibox(battery_widget)


function battery_widget:update()
	-- Update the icon
	local icon_function
	if bat_info.state == upower.state.CHARGING then
		icon_function = icons.create_battery_charging
	elseif bat_info.state == upower.state.DISCHARGING or bat_info.state == upower.state.FULLY_CHARGED then
		icon_function = icons.create_battery_discharging
	end
	self:get_children_by_id("icon")[1].image = icon_function(
		bat_info.percentage
	)

	-- Update the percentage (text)
	self:get_children_by_id("percentage")[1].text = string.format(
		"%d", bat_info.percentage
	)
end

if bat_info then
	battery_widget:update()
	upower.watch(
		function(name, value)
			if name == "State" then
				bat_info.state = value
				battery_widget:update()
			elseif name == "Percentage" then
				bat_info.percentage = value
				bat_info:notify_when_low(beautiful.battery_warn_threshold)
				battery_widget:update()
			elseif name == "TimeToEmpty" then
				bat_info.time_to_empty = value
			elseif name == "TimeToFull" then
				bat_info.time_to_full = value
			end
		end
	)
end

return battery_widget
