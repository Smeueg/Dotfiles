--------------------------------------------------------------------------------
--- Extra functions for awful.spawn
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local awful = require("awful")
local notify = require("naughty").notify
local get_command_path = require("gears").filesystem.get_command_path

--- Only spawn the command if it is installed. Else, notify the user
---@param command string The command to run
function awful.spawn.if_installed(command)
		local bin = command:match("^[^ ]+")
		if get_command_path(bin) then
			awful.spawn(command, false)
		else
		notify {
			title = "WARNING",
			text = string.format("'%s' isn't installed", bin)
		}
	end
end


--- Only spawn the command if it is installed. Else, notify the user
---@param command string The command to run
---@param callback function The callback to run
function awful.spawn.if_installed_easy_async(command, callback)
	local bin = command:match("^[^ ]+")
	if get_command_path(bin) then
		awful.spawn.easy_async(command, callback)
	else
		notify {
			title = "WARNING",
			text = string.format("'%s' isn't installed", bin)
		}
	end
end


--- Spawn the application while also notifying the user
---@param name string The text to display on the notification
---@param app string The applicatoin to launch
function awful.spawn.launch(name, app)
	notify { title = "Launching Application", text = name }
	awful.spawn(
		app,
		{ tag = awful.screen.focused().selected_tag }
	)
end
