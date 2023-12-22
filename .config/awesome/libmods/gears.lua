local gears = require("gears")

function gears.filesystem.get_command_path(command)
	for dir in string.gmatch(os.getenv("PATH") or "", "([^:]+)") do
		local path = dir .. "/" .. command
		if gears.filesystem.file_executable(path) then return path end
	end
	return nil
end
