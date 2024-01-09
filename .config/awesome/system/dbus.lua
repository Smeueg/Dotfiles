--------------------------------------------------------------------------------
--- Work with DBus within AwesomeWM
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Relevant Documentation:
--- * Gio:  https://docs.gtk.org/gio/
--- * GLib: https://docs.gtk.org/glib/
--------------------------------------------------------------------------------

local lgi = require("lgi")
local GLib, Gio = lgi.GLib, lgi.Gio

local dbus = {}
dbus.BusSYSTEM = Gio.bus_get_sync(Gio.BusType("SYSTEM"))


--- Parses a GLib.Variant
---@param v GLib.Variant
---@return table The parsed GLib Variant
function dbus.parse_variant(v)
	if v:is_container() then
		local t = {}
		for i = 0, v:n_children() - 1 do
			local value = v:get_child_value(i)
			t[i + 1] = dbus.parse_variant(value)
		end
		if #t == 1 then
			t = t[1]
		end
		return t
	else
		return v.value
	end
end


--- Calls a DBus method
---@param args table
---	* name            (string) : The bus name
---	* path            (string) : The object path
---	* method          (string) : The full string of the method
---                              (e.g. org.freedesktop.DBus.Properties.GetAll)
--- * parameters (GLib.Variant): The parameters to pass onto the method
function dbus.call(args)
	local _, i = args.method:find(".*[.]")
	local interface = args.method:sub(1, i-1)
	local method = args.method:sub(i+1)

	return dbus.parse_variant(
		dbus.BusSYSTEM:call_sync(
			args.name, -- bus_name
			args.path, -- object_path
			interface, -- interface_name
			method,
			args.parameters,
			nil, -- reply_type
			Gio.DBusCallFlags.NONE, -- flags
			-1 -- timeout_msec (-1: default, G_MAXINT: no_timeout)
		)
	)
end


--- Get a property from a DBus object
---@param args table
---  * name     (string) : The Bus name
---  * path     (string) : The Bus Object path
---  * property (string) : The full path of the property
---                        (e.g. org.freedesktop.NetworkManager.Devices)
---@return any A the property returned by the captured DBus object
function dbus.get_property(args)
	local _, i = args.property:find(".*[.]")
	local interface = args.property:sub(1, i-1)
	local property = args.property:sub(i+1)

	return dbus.call {
		name = args.name,
		path = args.path,
		method = "org.freedesktop.DBus.Properties.Get",
		parameters = GLib.Variant("(ss)", {interface, property})
	}
end


--- Get properties from a DBus object
---@param args table
---  * name      (string) : The Bus name
---  * path      (string) : The Bus Object path
---  * interface (string) : The interface
---
---@return table A table of properties from the captured DBus object
function dbus.get_properties(args)
		local bus = {}

		local properties = dbus.call {
			name = args.name,
			path = args.path,
			method = "org.freedesktop.DBus.Properties.GetAll",
			parameters = GLib.Variant("(s)", {args.interface})
	}

	for _, v in ipairs(properties) do
		bus[v[1]] = v[2]
	end

	return bus
end

return dbus
