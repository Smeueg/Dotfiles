--------------------------------------------------------------------------------
--- Extra functions for `wibox`
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
require("libmods.wibox.container")
local wibox = require("wibox")

local function cursor_change_to_hand()
	local w = mouse.current_wibox
	if w then
		w.cursor = "hand1"
	end
end

local function cursor_change_to_left_ptr()
	local w = mouse.current_wibox
	if w then
		w.cursor = "left_ptr"
	end
end

--- Change the cursor when hovering on a widget
---@param widget wibox.widget
function wibox.add_clickable(widget)
	widget:connect_signal("mouse::enter", cursor_change_to_hand)
	widget:connect_signal("mouse::leave", cursor_change_to_left_ptr)
end
