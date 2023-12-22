local awful = require("awful")

local brightness = {}

function brightness.modify(v)
	-- Type Check
	assert(type(v) == "number" and v % 1 == 0, "modify(v) expects an integer")

	local value_string = ""
	if v > 0 then
		value_string = string.format("+%d%%", v)
	else
		value_string = string.format("%d%%-", v * -1)
	end

	awful.spawn.if_installed(
		string.format("brightnessctl set %s", value_string)
	)
end

return brightness
