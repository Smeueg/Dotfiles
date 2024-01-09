--------------------------------------------------------------------------------
--- A NetworkManager widget for the Wibar
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------

local nm = require("system.nm")
local wibox = require("wibox")
local beautiful = require("beautiful")
local icon = require("ui.icons")

local network_widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.wibar_widget_shape,
	bg = "#00000030",
	{
		widget = wibox.container.background,
		layout = wibox.layout.fixed.horizontal,
		{
			widget = wibox.widget.imagebox,
			image = icon.offline,
			id = "icon",
		},
		{
			widget = wibox.widget.textbox,
			text = "- ",
			id = "text"
		}
	}
}


local function update_widget(connection)
	local text, image
	if connection then
		if connection.type == nm.DEVICE.TYPE.WIFI then
			image = icon.wifi
		elseif connection.type == nm.DEVICE.TYPE.ETHERNET then
			image = icon.eth
		end
		text = connection.id
	else
		image = icon.offline
		text = "-"
	end
	network_widget:get_children_by_id("text")[1].text = text .. " "
	network_widget:get_children_by_id("icon")[1].image = image
end

local active_connection = nm.get_active_connection()
if active_connection then
	update_widget(nm.get_active_connection())
	nm.watch(update_widget)
end

return network_widget
