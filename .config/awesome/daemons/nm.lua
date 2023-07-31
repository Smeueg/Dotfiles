--[[
	https://networkmanager.dev/docs/api/latest/
]]
local lgi = require("lgi")
local dbus = require("daemons.dbus")
local Gio = lgi.Gio

local nm = {
	DEVICE = {
		TYPE = {
			UNKNOWN = 0,
			GENERIC = 14,
			ETHERNET = 1,
			WIFI = 2,
			UNUSED1 = 3,
			UNUSED2 = 4,
			BT = 5,
			OLPC_MESH = 6,
			WIMAX = 7,
			MODEM = 8,
			INFINIBAND = 9,
			BOND = 10,
			VLAN = 11,
			ADSL = 12,
			BRIDGE = 13,
			TEAM = 15,
			TUN = 16,
			IP_TUNNEL = 17,
			MACVLAN = 18,
			VXLAN = 19,
			VETH = 20,
			MACSEC = 21,
			DUMMY = 22,
			PPP = 23,
			OVS_INTERFACE = 24,
			OVS_PORT = 25,
			OVS_BRIDGE = 26,
			WPAN = 27,
			["6LOWPAN"] = 28,
			WIREGUARD = 29,
			WIFI_P2P = 30,
			VRF = 31,
			LOOPBACK = 32
		},
		STATE = {
			UNKNOWN = 0,
			UNMANAGED = 10,
			UNAVAILABLE = 20,
			DISCONNECTED = 30,
			PREPARE = 40,
			CONFIG = 50,
			NEED_AUTH = 60,
			IP_CONFIG = 70,
			IP_CHECK = 80,
			SECONDARIES = 90,
			ACTIVATED = 100,
			DEACTIVATING = 110,
			FAILED = 120
		}
	},
	ACTIVE_CONNECTION = {
		STATE = {
			UNKNOWN = 0,
			ACTIVATING = 1,
			ACTIVATED = 2,
			DEACTIVATING = 3,
			DEACTIVATED = 4
		}
	}
}


--- Turns a table of bytes to a string
local function bytes_to_string(bytes)
	local str = ""
	for _, b in ipairs(bytes) do
		str = str .. string.char(b)
	end
	return str
end

local function get_connection_property(path)
	return dbus.get_properties {
		name = "org.freedesktop.NetworkManager",
		path = path,
		interface = "org.freedesktop.NetworkManager.Connection.Active"
	}
end


--- Gets the current active network using DBus
-- @return [table]
-- {
--  type (string): NetworkManager active connection type
--  id   (string): NetworkManager active connection id
-- }
function nm.get_active_connection()
	local active_connection
	local network = dbus.get_properties {
		name = "org.freedesktop.NetworkManager",
		path = "/org/freedesktop/NetworkManager",
		interface = "org.freedesktop.NetworkManager"
	}

	-- Use the path of PrimaryConnection if defined
	if network.PrimaryConnection ~= "/" then
		local ac = get_connection_property(network.PrimaryConnection)
		local device_type = dbus.get_property {
			name = "org.freedesktop.NetworkManager",
			path = ac.Devices,
			property = "org.freedesktop.NetworkManager.Device.DeviceType"
		}

		return {
			id = ac.Id,
			type = device_type
		}
	end

	-- Loop through activated devices if PrimaryConnection isn't defined
	for _, device_path in ipairs(network.Devices) do
		local device = dbus.get_properties {
			name = "org.freedesktop.NetworkManager",
			path = device_path,
			interface = "org.freedesktop.NetworkManager.Device"
		}

		if device.State == nm.DEVICE.STATE.ACTIVATED and device.DeviceType ~= nm.DEVICE.TYPE.LOOPBACK then
			local active_connection_path = dbus.get_property {
				name = "org.freedesktop.NetworkManager",
				path = device_path,
				property = "org.freedesktop.NetworkManager.Device.ActiveConnection"
			}

			local ac = get_connection_property(active_connection_path)

			active_connection = {
				id = ac.Id,
				type = device.DeviceType
			}

			if device.DeviceType == nm.DEVICE.TYPE.ETHERNET then
				return active_connection
			end
		end
	end

	return active_connection
end


--- Watches NetworkManager for active connection changes
-- @param callback[type=function]
-- callback should accept a table with the following structure:
-- {
--   id (string),
--   type (nm.DEVICE.TYPE)
-- }
function nm.watch_connections(callback)
	dbus.BusSYSTEM:signal_subscribe(
		"org.freedesktop.NetworkManager", -- sender
		"org.freedesktop.NetworkManager.Connection.Active", -- interface
		"StateChanged", -- member/signal
		nil, -- Object Path
		nil, -- arg0
		Gio.DBusSignalFlags.NONE, -- flags
		function(...) -- callback
			local user_data = ({...})[6]
			local state = user_data:get_child_value(0).value
			local nm_states = nm.ACTIVE_CONNECTION.STATE
			if state == nm_states.ACTIVATED or state == nm_states.DEACTIVATED then
				callback(nm.get_active_connection())
			end
		end
	)
end

-- nm.watch_connections(function(connection)
-- 		if connection then
-- 			print(string.format(
-- 					"Connected to: %s\nType: %s\n----",
-- 					connection.id,
-- 					connection.type
-- 			))
-- 		else
-- 			print("Not connected to anything")
-- 		end
-- end)


return nm
