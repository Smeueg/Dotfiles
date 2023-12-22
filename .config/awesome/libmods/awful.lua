local awful = require("awful")
local notify = require("naughty").notify
local gears = require("gears")

function awful.spawn.if_installed(command)
	local bin = command:match("^[^ ]+")
	if gears.filesystem.get_command_path(bin) then
		awful.spawn(command, false)
	else
		notify {
			title = "WARNING",
			text = string.format("'%s' isn't installed", bin)
		}
	end
end
