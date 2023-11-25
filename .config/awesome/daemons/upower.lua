local dbus = require("daemons.dbus")
local naughty = require("naughty")
local Gio = require("lgi").Gio

local upower = {}
upower.state = {
	UNKNOWN = 0,
	CHARGING = 1,
	DISCHARGING = 2,
	EMPTY = 3,
	FULLY_CHARGED = 4,
	PENDING_CHARGE = 5,
	PENDING_DISCHARGE = 6,
}


--- Retrieves the laptops battery info
-- @return [table]
-- {
-- 	percentage (int): The battery's current percentage
-- 	state (upower.state): The battery's current state
-- }
function upower.get_bat_info()
	local device = dbus.get_properties {
		name = "org.freedesktop.UPower",
		path = "/org/freedesktop/UPower/devices/DisplayDevice",
		interface = "org.freedesktop.UPower.Device"
	}

	return {
		percentage = device.Percentage,
		state = device.State,
		time_to_full = device.TimeToFull,
		time_to_empty = device.TimeToEmpty
	}
end

--- Watches UPower for battery percentage/state chcanges
-- @param callback[type=function]
-- callback should accept a table with the following structure:
-- {
--   name (string),
--   value (can be anthing)
-- }
function upower.watch_battery(callback)
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
				callback {
					name = property_name,
					value = property_value
				}
			end
		end
	)
end

function upower.show_time()
	if upower.notification then
		naughty.destroy(upower.notification)
		upower.notification = nil
	end
	local info = upower.get_bat_info()
	local time_seconds = 0
	local annotation
	local text
	if info.time_to_full ~= 0 then
		time_seconds = info.time_to_full
		annotation = "full"
	elseif info.time_to_empty ~= 0 then
		time_seconds = info.time_to_empty
		annotation = "empty"
	end
	local time_minutes = math.floor(time_seconds / 60)
	if time_minutes > 60 then
		local time_hours = math.floor(time_minutes / 60)
		text = string.format(
			"%s hours and %s minutes 'til %s", time_hours,
			time_minutes % 60,
			annotation
		)
	else
		text = string.format("%s minutes 'til %s", time_minutes, annotation)
	end
	upower.notification = naughty.notify {
		title = "Battery",
		text = text
	}
end

return upower
