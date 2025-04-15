local gears = require("gears")
local Gio = require("lgi").Gio

local oldrequire = require
require = setmetatable(
	{
		dir = function(dir)
			local modules =  {}
			local iterator = Gio.File.new_for_path(
				gears.filesystem.get_configuration_dir()..dir
			):enumerate_children("standard::name", "NONE")

			while true do
				local info = iterator:next_file()
				if not info then break end

				local file = info:get_name()
				if file ~= "init.lua" then
					file = file:gsub(".lua$", "")
					modules[file] = oldrequire(dir.."."..file)
				end
			end

			return modules
		end
	},
	{
		__call = function(_, path) return oldrequire(path) end
	}
)


require.dir("libmods")
