local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local wibox = require("wibox")
local dpi = beautiful.xresources.apply_dpi

function awful.widget.tasklist_styled(s)
	return awful.widget.tasklist {
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		style = { shape = gears.shape.rounded_rect_auto },
		buttons = gears.table.join(
			awful.button({}, 1, function(c)
					if c == client.focus then
						c.minimized = true
					else
						c:emit_signal(
							"request::activate",
							"tasklist",
							{raise = true}
						)
					end
			end),
			awful.button({}, 3, function(c)
					local menu = awful.menu {
						{"Close", function() c:kill() end, nil}
					}
					menu:show()

					-- Hide and destroy the menu
					menu.wibox.widget:connect_signal("mouse::leave", function()
							menu:hide()
							menu = nil
					end)
			end),
			awful.button({ }, 4, function() awful.client.focus.byidx(1) end),
			awful.button({ }, 5, function() awful.client.focus.byidx(-1) end)
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
						id = "icon_role",
					}
				}
			},
			create_callback = function(self, c)
				local client = c
				while not client.icon_sizes[1] do
					client = client.transient_for
				end
				self:get_children_by_id("icon_role")[1].image = client.icon
			end
		}
	}
end
