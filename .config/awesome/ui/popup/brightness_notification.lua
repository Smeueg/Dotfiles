--------------------------------------------------------------------------------
--- Notify the current brightness percentage to the user
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--- 
--- Relevant Documentation:
--------------------------------------------------------------------------------
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local awful = require("awful")
local icons = require("ui.icons")
local dpi = beautiful.xresources.apply_dpi
local percentage = beautiful.popup_percentage or 0.1
local icon_size = beautiful.brightness_notification_icon_size or 50
local delay = beautiful.brightness_notification_delay
local popup = nil
local popup_timer = nil



local function destroy_popup()
	popup.visible = false
	popup = nil
	popup_timer:stop()
	popup_timer = nil
end


local function create_popup()
	local screen_geometry = awful.screen.focused().geometry
	local size = math.min(screen_geometry.width, screen_geometry.height) * percentage
	popup = awful.popup {
		shape = gears.shape.rounded_rect_auto,
		border_width = beautiful.border_width,
		border_color = beautiful.border_focus,
		placement = awful.placement.centered,
		ontop = true,
		visible = false,
		widget = {
			widget = wibox.container.constraint,
			forced_width = size,
			forced_height = size,
			{
				widget = wibox.container.place,
				{
					layout = wibox.layout.fixed.vertical,
					spacing = dpi(5),
					{
						widget = wibox.container.place,
						{
							widget = wibox.widget.imagebox,
							image = icons.brightness,
							forced_width = icon_size,
							forced_height = icon_size
						}
					},
					{
						widget = wibox.widget.progressbar,
						id = "progressbar",
						max_value = 100,
						value = 10,
							border_color = beautiful.fg_normal,
							color = beautiful.fg_normal,
						background_color = "#00000000",
						border_width = dpi(1),
						forced_width = size * 0.8,
						forced_height = icon_size / 5,
					}
				}
			}
		}
	}

    popup_timer = gears.timer {
        timeout = 1,
        autostart = true,
		callback = destroy_popup
	}

	popup.visible = true
end


local function update_popup(value)
	popup.widget:get_children_by_id("progressbar")[1].value = value
end


local function notify(stdout)
	if not popup then
		create_popup()
	end
	update_popup(tonumber(stdout:match("(%d*)%%")))
	popup_timer:again()
end

return notify
