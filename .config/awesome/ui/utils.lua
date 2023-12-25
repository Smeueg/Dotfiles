local wibox = require("wibox")
local beautiful = require("beautiful")
local utils = {}

function utils.border_wrapper(w, disabled)
	disabled = disabled or {}
	local function border(orientation, position)
		if disabled[position] then
			return { widget = wibox.container.background }
		end

		local attr = orientation == "vertical" and "width" or "height"
		return {
			widget = wibox.container.background,
			bg = beautiful.border_focus,
			{
				widget = wibox.container.constraint,
				["forced_"..attr] = beautiful.border_width,
			}
		}
	end

	return {
		widget = wibox.container.background,
		{
			layout = wibox.layout.align.vertical,
			border(nil, "top"),
			{
				layout = wibox.layout.align.horizontal,
				border("vertical", "left"),
				w,
				border("vertical", "right"),
			},
			border(nil, "bottom")
		}
	}
end

return utils
