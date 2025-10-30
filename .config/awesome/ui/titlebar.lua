local cairo = require("lgi").cairo
local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local gshape = gears.shape
local gtable = gears.table
local beautiful = require("beautiful")
local apply_dpi = beautiful.xresources.apply_dpi
local diameter = apply_dpi(15)

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
---@param client client
---@return wibox.widget
local function create_btn(color, callback)
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
		local max_button = create_btn(
			beautiful.titlebar_btn_max,
			function() c.maximized = not c.maximized end
		)
		local min_button = create_btn(
			beautiful.titlebar_btn_min,
			function() c.minimized = not c.minimized end
		)
		local close_button = create_btn(
			beautiful.titlebar_btn_close,
			function() c:kill() end
		)

		local titlebar = awful.titlebar(c, { size = titlebar_height })

		titlebar:setup {
			widget = wibox.container.background,
			shape = gshape.rectangle,
			shape_border_width = beautiful.border_width,
			shape_border_color = beautiful.border_focus,
			{
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
						max_button,
						min_button,
						close_button
					}
				}
			}
		}

		local highlight_decorator = function()
			max_button.bg = beautiful.titlebar_btn_max
			min_button.bg = beautiful.titlebar_btn_min
			close_button.bg = beautiful.titlebar_btn_close
			titlebar.widget.shape_border_color = beautiful.border_focus
		end

		local unhighlight_decorator = function()
			max_button.bg = beautiful.border_normal
			min_button.bg = beautiful.border_normal
			close_button.bg = beautiful.border_normal
			titlebar.widget.shape_border_color = beautiful.border_normal
		end

		c:connect_signal("focus", highlight_decorator)
		c:connect_signal("unfocus", unhighlight_decorator)

		titlebar.widget:connect_signal("mouse::enter", highlight_decorator)
		titlebar.widget:connect_signal(
			"mouse::leave",
			function ()
				if c ~= client.focus then
					unhighlight_decorator()
				end
			end
		)

		if c ~= client.focus then
			unhighlight_decorator()
		end
end)
