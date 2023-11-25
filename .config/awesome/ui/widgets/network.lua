local wibox = require("wibox")
local beautiful = require("beautiful")
local icon = require("ui.icons")

local widget = wibox.widget {
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

local widget_text = widget:get_children_by_id("text")[1]

local widget_icon = widget:get_children_by_id("icon")[1]

-- Initial Setup
local NM = require("daemons.nm")

local function display_connection(connection)
	local text
	local image
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

display_connection(NM.get_active_connection())
NM.watch_connections(display_connection)

return widget
