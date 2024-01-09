--------------------------------------------------------------------------------
--- Work with UPower using DBus for AwesomeWM
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Relevant Documentation:
--- * https://upower.freedesktop.org/docs/ref-dbus.html
--------------------------------------------------------------------------------
local dbus = require("system.dbus")
local naughty = require("naughty")
local Gio = require("lgi").Gio

local GlobalTimeNotification
local GlobalLowBatNotification
local upower = {}


---@class BatState
upower.state = {
	UNKNOWN = 0,
	CHARGING = 1,
	DISCHARGING = 2,
	EMPTY = 3,
	FULLY_CHARGED = 4,
	PENDING_CHARGE = 5,
	PENDING_DISCHARGE = 6,
}


---@class BatInfo
---@field percentage number
---@field state BatState
---@field time_to_full number In seconds
---@field time_to_empty number In seconds
local BatInfo = setmetatable({}, {
		--- Creates a new BatInfo instance
		---@param args table
		--- * Percentage (number)
		--- * state (number/upower.state)
		--- * time_to_full (number)
		--- * time_to_empty (number)
		---@return BatInfo
		__call = function(self, args)
			return setmetatable(
				{
					percentage = args.Percentage,
					state = args.State,
					time_to_full = args.TimeToFull,
					time_to_empty = args.TimeToEmpty
				},
				{ __index = self }
			)
		end
})


--- Sends a warning notification if the battery percentage is below a certain
--- threshold
---@param threshold number
function BatInfo:notify_when_low(threshold)
	if self.percentage <= threshold and self.state == upower.state.DISCHARGING then
		GlobalLowBatNotification = naughty.notify {
			title = "Warning",
			text = string.format("Your battery is at %d%%", self.percentage)
		}
	elseif GlobalLowBatNotification then
		naughty.destroy(GlobalLowBatNotification, nil)
		GlobalLowBatNotification = nil
	end
end

--- Sends a notification of the battery's progress,
--- could either be the time till it's full or the time till it's empty
--- depending whether it's currently charging or not
function BatInfo:notify_time()
	local notification_suffix, notification_text, seconds

	if GlobalTimeNotification then
		naughty.destroy(GlobalTimeNotification)
		GlobalTimeNotification = nil
	end

	if self.time_to_full ~= 0 then
		seconds = self.time_to_full
		notification_suffix = "full"
	elseif self.time_to_empty ~= 0 then
		seconds = self.time_to_empty
		notification_suffix = "empty"
	else
		return
	end

	local hours = math.floor(seconds / 3600)
	local minutes = math.floor(seconds % 3600 / 60)

	if hours == 0 then
		notification_text = string.format(
			"%d minutes 'till %s",
			minutes, notification_suffix
		)
	else
		notification_text = string.format(
			"%s hours and %s minutes 'till %s",
			hours, minutes, notification_suffix
		)
	end

	GlobalTimeNotification = naughty.notify {
		title = "Battery",
		text = notification_text
	}
end

--- Watches UPower for battery percentage/state chcanges
---@param callback function(name:string, value)
function upower.watch(callback)
	dbus.BusSYSTEM:signal_subscribe(
		"org.freedesktop.UPower",                  -- sender
		"org.freedesktop.DBus.Properties",         -- interface
		"PropertiesChanged",                       -- member/signal
		"/org/freedesktop/UPower/devices/DisplayDevice", -- Object Path
		nil,                                       -- arg0
		Gio.DBusSignalFlags.NONE,                  -- flags
		function(...)                              -- callback
			local changed_properties = ({ ... })[6]:get_child_value(1)
			for i = 0, changed_properties:n_children() - 1 do
				local property = changed_properties:get_child_value(i)
				local property_name = property:get_child_value(0).value
				local property_value = property:get_child_value(1).value.value
				callback(property_name, property_value)
			end
		end
	)
end

--- Gets the current battery information using DBus
---@return BatInfo|nil The current battery information
function upower.get_info()
	local properties = dbus.get_properties {
		name = "org.freedesktop.UPower",
		path = "/org/freedesktop/UPower/devices/DisplayDevice",
		interface = "org.freedesktop.UPower.Device"
	}
	if not properties then return end
	return BatInfo(
		dbus.get_properties {
			name = "org.freedesktop.UPower",
			path = "/org/freedesktop/UPower/devices/DisplayDevice",
			interface = "org.freedesktop.UPower.Device"
		}
	)
end

return upower
