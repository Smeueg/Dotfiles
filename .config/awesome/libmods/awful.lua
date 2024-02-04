--------------------------------------------------------------------------------
--- Extra functions for Awful
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local awful = require("awful")
local notify = require("naughty").notify
local gears = require("gears")

--- Only spawn the command if it is installed. Else, notify the user
---@param command string The command to run
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

--- Set the layout for every tag
---@param layout awful.layout
function awful.layout.set_all(layout)
	for _, t in ipairs(root.tags()) do
		awful.layout.set(layout, t)
	end
end

--- Increase the master-width-factor for all tags
---@param factor number
function awful.layout.incmwf_all(factor)
	if awful.layout.get() ~= awful.layout.suit.tile.right then return end
	for _, t in ipairs(root.tags()) do awful.tag.incmwfact(factor, t) end
end

--- Increment the number of master windows for all tags
---@param n number
function awful.layout.incnmaster(n)
	if awful.layout.get() ~= awful.layout.suit.tile.right then return end
	if root.tags()[1].master_count == 1 and n < 0 then return end
	for _, t in ipairs(root.tags()) do awful.tag.incnmaster(n, t, true) end
	notify {
		title = "Current Master Count",
		text = tostring(root.tags()[1].master_count),
		position = "top_left",
		timeout = 2
	}
end
