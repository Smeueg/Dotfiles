local awful = require("awful")
local notify = require("naughty").notify
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

-- Spawn functions
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

function awful.spawn.launch(name, cmd)
	notify { title = "Launching Application", text = name }
	awful.spawn(cmd, {
			tag = awful.screen.focused().selected_tag
	})
end

-- Layout functions
function awful.layout.set_all(layout)
	-- Sets a layout for every tag
	for _, t in ipairs(root.tags()) do
		awful.layout.set(layout, t)
	end
end

function awful.layout.incmwf_all(factor)
	if awful.layout.get() ~= awful.layout.suit.tile.right then return end
	for _, t in ipairs(root.tags()) do awful.tag.incmwfact(factor, t) end
end

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
