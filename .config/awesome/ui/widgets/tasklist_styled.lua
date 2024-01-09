--------------------------------------------------------------------------------
--- A custom tasklist for the Wibar
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Relevant Documentation:
--- * https://awesomewm.org/doc/api/classes/awful.widget.tasklist.html
--- * https://awesomewm.org/doc/api/classes/client.html
--------------------------------------------------------------------------------
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local wibox = require("wibox")
local icons = require("ui.icons")
local dpi = beautiful.xresources.apply_dpi
local capi = { client = client }

local function tasklist_styled(s)
	return awful.widget.tasklist {
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		style = { shape = gears.shape.rounded_rect_auto },
		buttons = gears.table.join(
			awful.button({}, 1, function(c)
					if c == capi.client.focus then
						c.minimized = true
					else
						c:emit_signal(
							"request::activate",
							"tasklist",
							{ raise = true }
						)
					end
			end),
			awful.button({}, 3, function(c)
					local menu = awful.menu {
						{ "Close", function() c:kill() end, nil }
					}
					menu:show()

					-- Hide and destroy the menu
					menu.wibox.widget:connect_signal("mouse::leave", function()
							menu:hide()
							menu = nil
					end)
			end),
			awful.button({}, 4, function() awful.client.focus.byidx(1) end),
			awful.button({}, 5, function() awful.client.focus.byidx(-1) end)
		),
		widget_template = {
			widget = wibox.container.background,
			id = "background_role",
			{
				widget = wibox.container.margin,
				margins = dpi(beautiful.tasklist_inner_margin),
				{
					widget = wibox.container.place,
					halign = "center",
					{
						widget = wibox.widget.imagebox,
						id = "imagebox",
					}
				}
			},
			create_callback = function(self, c)
				local client = c
				local icon

				if client.icon_sizes[1] ~= nil then
					while client.icon_sizes[1] == nil do
						client = client.transient_for
					end
					icon = client.icon
				else
					icon = icons.tasklist_no_icon
				end

				self:get_children_by_id("imagebox")[1].image = icon
			end
		}
	}
end
return tasklist_styled
