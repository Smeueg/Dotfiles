local lgi = require("lgi")
local gears = require("gears")
local notify = require("naughty").notify
local Gio = lgi.Gio

local dir = Gio.File.new_for_path(gears.filesystem.get_configuration_dir().."ui/widgets/")
local enum = dir:enumerate_children("standard::name", "NONE")

local tmp = {}
while true do
	local info = enum:next_file()
	if info then
		local file = info:get_name()
		if file ~= "init.lua" then
			file = file:gsub(".lua$", "")
			tmp[file] = require("ui.widgets."..file)
		end
	else
		break
	end
end

return tmp
