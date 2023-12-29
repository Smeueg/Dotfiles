local upower    = require("daemons.upower")
local beautiful = require("beautiful")
local wibox     = require("wibox")
local awful     = require("awful")
local icons     = require("ui.icons")

local bat_info = upower.get_info()

local battery_widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.shape_universal,
	bg = "#00000030",
	buttons = awful.button({}, 1, function()
			if bat_info then
				bat_info:notify_time()
			end
	end),
	{
		widget = wibox.container.background,
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


--- Updates the icon for the battery widget
---@param state BatState
function battery_widget:update_icon(state)
	local icon = self:get_children_by_id("icon")[1]
	if state == upower.state.CHARGING then
		icon.image = icons.battery_charging
	elseif state == upower.state.DISCHARGING then
		icon.image = icons.battery_discharging
	elseif state == upower.state.FULLY_CHARGED then
		icon.image = icons.battery_fully_charged
	end
end


--- Updates the percentage string for the battery widget
---@param percentage number
function battery_widget:update_percentage(percentage)
	self:get_children_by_id("percentage")[1].text = string.format(
		"%d", percentage
	)
end


if bat_info then
	battery_widget:update_icon(bat_info.state)
	battery_widget:update_percentage(bat_info.percentage)

	upower.watch(
		function(name, value)
			if name == "State" then
				bat_info.state = value
				battery_widget:update_icon(value)
			elseif name == "Percentage" then
				battery_widget:update_percentage(value)
				bat_info.percentage = value
				bat_info:notify_when_low(15)
			end
		end
	)
end


return battery_widget
