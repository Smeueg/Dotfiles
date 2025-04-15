--------------------------------------------------------------------------------
--- Extra functions for gears
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local gears = require("gears")
local apply_dpi = require("beautiful").xresources.apply_dpi

--- Get the path of an installed command/program
---@param command string
---@return string|nil
function gears.filesystem.get_command_path(command)
	for dir in string.gmatch(os.getenv("PATH") or "", "([^:]+)") do
		local path = dir .. "/" .. command
		if gears.filesystem.file_executable(path) then return path end
	end
	return nil
end

--- Create a rounded rectangle with a roundness of apply_dpi(5)
---@param cr cairo.Context
---@param width number
---@param height number
function gears.shape.rounded_rect_auto(cr, width, height)
	gears.shape.rounded_rect(cr, width, height, apply_dpi(5))
end
