--------------------------------------------------------------------------------
--- Work with NetworkManager using DBus within AwesomeWM
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Relevant Documentation:
--- * https://networkmanager.dev/docs/api/latest/
--------------------------------------------------------------------------------
local lgi = require("lgi")
local dbus = require("system.dbus")
local Gio = lgi.Gio

local nm = {
	---@class NetworkManagerDeviceType
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


---@class NetworkManagerConnection
---@field id string
---@field type NetworkManagerDeviceType

--- Creates a NetworkManagerConnection instance
---@param id string
---@param type NetworkManagerDeviceType
---@return NetworkManagerConnection
local function NetworkManagerConnection(id, type)
	return {
		id = id,
		type = type
	}
end


--- Gets aconnection from a path
---@param path string The path to the active connection
local function get_connection(path)
	local connection = dbus.get_properties {
		name = "org.freedesktop.NetworkManager",
		path = path,
		interface = "org.freedesktop.NetworkManager.Connection.Active"
	}

	local device_type = dbus.get_property {
		name = "org.freedesktop.NetworkManager",
		path = connection.Devices,
		property = "org.freedesktop.NetworkManager.Device.DeviceType"
	}

	return NetworkManagerConnection(connection.Id, device_type)
end


--- Gets the current active network using DBus
---@return NetworkManagerConnection
function nm.get_active_connection()
	local active_connection
	local network = dbus.get_properties {
		name = "org.freedesktop.NetworkManager",
		path = "/org/freedesktop/NetworkManager",
		interface = "org.freedesktop.NetworkManager"
	}

	-- Use the path of PrimaryConnection if defined
	if network.PrimaryConnection ~= "/" then
		return get_connection(network.PrimaryConnection)
	end

	-- Loop through activated devices if PrimaryConnection isn't defined
	for _, device_path in ipairs(network.Devices) do
		local device = dbus.get_properties {
			name = "org.freedesktop.NetworkManager",
			path = device_path,
			interface = "org.freedesktop.NetworkManager.Device"
		}

		local device_is_activated = (device.State == nm.DEVICE.STATE.ACTIVATED)
		local device_is_loopback = (device.DeviceType == nm.DEVICE.TYPE.LOOPBACK)
		if device_is_activated and not device_is_loopback then
			local active_connection_path = dbus.get_property {
				name = "org.freedesktop.NetworkManager",
				path = device_path,
				property = "org.freedesktop.NetworkManager.Device.ActiveConnection"
			}

			local device_id = dbus.get_property {
				name = "org.freedesktop.NetworkManager",
				path = active_connection_path,
				property = "org.freedesktop.NetworkManager.Connection.Active.Id"
			}

			active_connection = NetworkManagerConnection(
				device_id,
				device.DeviceType
			)

			if device.DeviceType == nm.DEVICE.TYPE.ETHERNET then
				return active_connection
			end
		end
	end

	return active_connection
end

--- Watches NetworkManager for active connection changes
---@param callback fun(type: NetworkManagerConnection)
function nm.watch(callback)
	dbus.BusSYSTEM:signal_subscribe(
		"org.freedesktop.NetworkManager",             -- sender
		"org.freedesktop.NetworkManager.Connection.Active", -- interface
		"StateChanged",                               -- member/signal
		nil,                                          -- Object Path
		nil,                                          -- arg0
		Gio.DBusSignalFlags.NONE,                     -- flags
		function(...)                                 -- callback
			local user_data = ({ ... })[6]
			local state = user_data:get_child_value(0).value
			local nm_states = nm.ACTIVE_CONNECTION.STATE
			if state == nm_states.ACTIVATED or state == nm_states.DEACTIVATED then
				callback(nm.get_active_connection())
			end
		end
	)
end

return nm
