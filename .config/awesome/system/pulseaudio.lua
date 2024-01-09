--------------------------------------------------------------------------------
--- A wrapper for PulseAudio for use in AwesomeWM
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Relevant Documentation:
--- * pactl(1)
--------------------------------------------------------------------------------

local awful = require("awful")

local pulseaudio = {}

function pulseaudio.modify_vol(v)
	-- Type Check
	assert(
		type(v) == "number" and v % 1 == 0,
		"modify_vol(v) expects an integer"
	)

	awful.spawn.if_installed(
		string.format(
			"pactl set-sink-volume @DEFAULT_SINK@ %s%d%%",
			v > 0 and "+" or "",
			v
		)
	)
end

function pulseaudio.toggle_mute()
	awful.spawn.if_installed("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
end

return pulseaudio
