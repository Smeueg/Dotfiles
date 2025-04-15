local cairo = require("lgi").cairo
local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local gshape = gears.shape
local gtable = gears.table
local beautiful = require("beautiful")
local apply_dpi = beautiful.xresources.apply_dpi

--- Handler for titlebar buttons
---@param widget wibox.container.background
local function enter_signal_handler(widget)
	widget.bg = cairo.surface_to_rgba(widget.bg):sub(0, 7) .. "90"
end

--- Handler for titlebar buttons
---@param widget wibox.container.background
local function leave_signal_handler(widget)
	widget.bg = cairo.surface_to_rgba(widget.bg):sub(0, 7)
end

-- Create a circular button
---@param color string
---@param callback function(c: client)
---@return wibox.widget
local function create_btn(color, callback)
	local diameter = apply_dpi(15)
	local btn = wibox.widget {
		buttons = awful.button({}, 1, callback),
		widget = wibox.container.background,
		shape = gshape.circle,
		bg = color,
		{
			widget = wibox.container.constraint,
			forced_height = diameter,
			forced_width = diameter
		}
	}

	btn:connect_signal("mouse::enter", enter_signal_handler)
	btn:connect_signal("mouse::leave", leave_signal_handler)

	return btn
end

client.connect_signal("request::titlebars", function(c)
		local titlebar_height = beautiful.titlebar_height or apply_dpi(40)
		awful.titlebar(c, { size = titlebar_height }):setup {
			widget = wibox.container.margin,
			margins = apply_dpi(10),
			{
				layout = wibox.layout.align.horizontal,
				{
					widget = wibox.widget.imagebox,
					image = c.icon
				},
				{
					layout = wibox.layout.flex.horizontal,
					buttons = gtable.join(
						awful.button({}, 1, function()
								awful.mouse.client.move(c)
						end),
						awful.button({}, 3, function()
								awful.mouse.client.resize(c)
						end)
					)
				},
				{
					widget = wibox.container.background,
					layout = wibox.layout.fixed.horizontal,
					spacing = apply_dpi(10),
					create_btn(beautiful.titlebar_btn_max, function()
							c.maximized = not c.maximized
					end),
					create_btn(beautiful.titlebar_btn_min, function()
							c.minimized = not c.minimized
					end),
					create_btn(beautiful.titlebar_btn_close, function()
							c:kill()
					end)
				}
			}
		}
end)
