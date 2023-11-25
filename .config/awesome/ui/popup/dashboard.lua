local utils = require("ui.utils")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local icon = require("ui.icons")
local dpi = beautiful.xresources.apply_dpi

local icon_size = dpi(25)
local scrollbar_width = dpi(10)
local widget_outer_spacing = dpi(10)
local widget_spacing = dpi(5)
local separator_spacing = dpi(1)


local sections = {
	power = nil,
	launcher = nil
}

local popup = awful.popup {
	ontop = true,
	visible = false,
	widget = utils.border_wrapper({
			widget = wibox.container.margin,
			margins = widget_outer_spacing,
			{
				layout = wibox.layout.fixed.horizontal,
				spacing = separator_spacing,
				spacing_widget = {
					widget = wibox.widget.separator,
					color = "#ffffff10"
				},
				sections.power,
				sections.launcher
				-- {
				-- 	widget = wibox.container.margin,
				-- 	margins = dpi(10),
				-- 	id = "power"
				-- },
				-- {
				-- 	widget = wibox.container.margin,
				-- 	margins = dpi(10),
				-- 	id = "launcher"
				-- }
			}
	}, {top = true, left = true})
}

sections.power = wibox.widget {
	layout = wibox.layout.fixed.vertical,
	spacing = widget_spacing,
	{
		layout = wibox.layout.fixed.horizontal,
		spacing = widget_spacing,
		{ -- search icon
			widget = wibox.widget.imagebox,
			image = icon.search,
			forced_height = icon_size,
			forced_width = icon_size
		},
		{
			widget = wibox.widget.textbox,
			id = "searchbar"
		}
	},
	{
		layout = wibox.layout.fixed.horizontal,
		{
			layout = wibox.layout.grid,
			homogeneous = false,
			id = "grid"
		},
		{
			widget = wibox.widget.imagebox,
			forced_width = scrollbar_width
		}
	}
}
