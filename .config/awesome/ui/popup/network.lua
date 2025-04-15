--------------------------------------------------------------------------------
--- Popup to connect to a network
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Relevant Documentation:
--- * https://awesomewm.org/doc/api/classes/awful.popup.html
--------------------------------------------------------------------------------
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local apply_dpi = beautiful.xresources.apply_dpi
local nm = require("system.nm")


--- Place a widget on the top right of the screen but below the wibar
---@param widget wibox.widget
local function place_top_right(widget)
	widget.visible = false
	awful.placement.next_to(
		widget,
		{
			preferred_positions = "bottom",
			preferred_anchors = "back",
			geometry = awful.screen.focused().wibar
		}
	)

	widget.y = widget.y - beautiful.border_width
	widget.visible = true
end

---@class SsidWidget
local SsidWidget = {}

--- Create a widget from an ssid
---@param wifi_access_point WifiAccessPoint
---@return SsidWidget
function SsidWidget.new(wifi_access_point, is_active)
	local icon_map = {
		[0] = "󰤯",
		[1] = "󰤟",
		[2] = "󰤢",
		[3] = "󰤥",
		[4] = "󰤨",
	}

	local markup = string.format(
		"%s %s  %s",
		is_active and "*" or " ",
		icon_map[math.floor(wifi_access_point.strength / 25)],
		wifi_access_point.ssid
	)

	return wibox.widget {
		widget = wibox.widget.textbox,
		markup = markup
	}
end

---@class NetworkPopup
---@field popup awful.popup
local NetworkPopup = {}
NetworkPopup.mt = {__index = NetworkPopup}

---@type NetworkPopup|nil
local global_popup

function NetworkPopup.new()
	local popup = awful.popup {
		widget = {
            widget = wibox.container.border,
			borders = {"left", "right"},
			{
				widget = wibox.container.margin,
				margins = apply_dpi(20),
				{
					layout = wibox.layout.fixed.vertical,
					spacing = apply_dpi(20),
					{
						widget = wibox.widget.textbox,
						text = "Available Networks:"
					},
					{
						widget = wibox.widget.separator,
						id = "separator",
						forced_height = apply_dpi(3),
						forced_width = 0,
					},
					{
						layout = wibox.layout.fixed.vertical,
						spacing = apply_dpi(10),
						id = "wifi_list",
						{
							widget = wibox.widget.textbox,
							text = "Loading..."
						}
					}
				}
			}
		},
		ontop = true,
        placement = place_top_right,
	}

	local separator_widget = popup.widget:get_children_by_id("separator")[1]
	separator_widget.forced_width = popup.width - apply_dpi(20) - beautiful.border_width

	return setmetatable({popup = popup}, NetworkPopup.mt)
end

function NetworkPopup:update_wifi_list()
	local wifi_list = self.popup.widget:get_children_by_id("wifi_list")[1]
	local currently_connected_ssid = nm.get_active_connection().id

	wifi_list:reset()
	for _, wifi_access_point in ipairs(nm.WifiAccessPoint.get_all()) do
		wifi_list:add(SsidWidget.new(wifi_access_point, wifi_access_point.ssid == currently_connected_ssid))
	end
end

--- Toggle the NetworkPopup
function NetworkPopup.toggle()
	if global_popup ~= nil then
		global_popup.popup.visible = false
		global_popup = nil
		return
	end

	global_popup = NetworkPopup.new()
	-- global_popup:update_wifi_list()
	-- nm.WifiAccessPoint.rescan(function() global_popup:update_wifi_list() end)
end

return { toggle = NetworkPopup.toggle }
