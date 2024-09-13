--------------------------------------------------------------------------------
--- A wrapper for brightnessctl for use in AwesomeWM
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Relevant Documentation:
--- * brightnessctl(1)
--------------------------------------------------------------------------------
local awful = require("awful")
local brightness_notification = require("ui.popup.brightness_notification")

local brightness = {}

--- Change the current brightness of the screen with `brightnessctl`
---@param v number The number used to decrement the brightness 
function brightness.modify(v)
	-- Type Check
	assert(type(v) == "number" and v % 1 == 0, "modify(v) expects an integer")

	local value_string
	if v > 0 then
		value_string = string.format("+%d%%", v)
	else
		value_string = string.format("%d%%-", v * -1)
	end

	awful.spawn.if_installed_easy_async(
        "brightnessctl set " .. value_string,
		brightness_notification
	)
end

return brightness
