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
local awful = require("awful")
local network_toggle_popup = require("ui.popup.network")

local network_widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.wibar_widget_shape,
	bg = "#00000030",
	buttons = awful.button({}, 1, network_toggle_popup.toggle),
	id = "wibar_network",
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


if nm.is_active() then
	nm.watch(update_widget)
end

nm.run_when_loaded(
	function()
		nm.watch(update_widget)
	end
)

return network_widget
