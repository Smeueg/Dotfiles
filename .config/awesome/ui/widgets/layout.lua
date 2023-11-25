local beautiful = require("beautiful")
local icon = require("ui.icons")
local wibox = require("wibox")
local awful = require("awful")

local capi = {
	root = root,
	tag = tag
}


local widget = wibox.widget {
	widget = wibox.container.background,
	shape = beautiful.shape_universal,
	bg = "#00000030",
	buttons = awful.button({}, 1, function()
			local layout = capi.root.tags()[1].layout
			if layout == awful.layout.suit.tile.right then
				awful.layout.set_all(awful.layout.suit.max)
			elseif layout == awful.layout.suit.max then
				awful.layout.set_all(awful.layout.suit.floating)
			else
				awful.layout.set_all(awful.layout.suit.tile.right)
			end
	end),
	{ widget = wibox.widget.imagebox }
}

local function update(t)
	if t.index > 1 then return end
	if t.layout == awful.layout.suit.tile.right then
		widget:get_children()[1].image = icon.tile
	elseif t.layout == awful.layout.suit.max then
		widget:get_children()[1].image = icon.max
	else
		widget:get_children()[1].image = icon.float
	end
end

capi.tag.connect_signal("property::layout", update)

return widget
