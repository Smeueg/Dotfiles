local gears = require("gears")
local dpi = require("beautiful").xresources.apply_dpi

function gears.filesystem.get_command_path(command)
	for dir in string.gmatch(os.getenv("PATH") or "", "([^:]+)") do
		local path = dir .. "/" .. command
		if gears.filesystem.file_executable(path) then return path end
	end
	return nil
end

function gears.shape.rounded_rect_auto(cr, width, height)
	gears.shape.rounded_rect(cr, width, height, dpi(5))
end
