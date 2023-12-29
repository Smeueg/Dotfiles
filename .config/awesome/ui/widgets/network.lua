local wibox = require("wibox")
local beautiful = require("beautiful")
local icon = require("ui.icons")

local network_widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.shape_universal,
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


local widget_text = network_widget:get_children_by_id("text")[1]
local widget_icon = network_widget:get_children_by_id("icon")[1]

-- Initial Setup
local NM = require("daemons.nm")

local function update_widget(connection)
	local text, image
	if connection then
		if connection.type == NM.DEVICE.TYPE.WIFI then
			image = icon.wifi
		elseif connection.type == NM.DEVICE.TYPE.ETHERNET then
			image = icon.eth
		end
		text = connection.id
	else
		image = icon.offline
		text = "-"
	end
	widget_text.text = text .. " "
	widget_icon.image = image
end

update_widget(NM.get_active_connection())
NM.watch(update_widget)

return network_widget
