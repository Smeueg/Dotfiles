--------------------------------------------------------------------------------
--- A library to change cursor shapes
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Relevant Documentation:
--- * https://awesomewm.org/doc/api/libraries/root.html#cursor
--------------------------------------------------------------------------------
local cursor = {}

--- Automatically add signals to change the cursor when a wibox is hovered
---@param wibox wibox
function cursor.add_clickable_to_wibox(wibox)
	wibox:connect_signal("mouse::enter", cursor.set_to_hand1)
	wibox:connect_signal("mouse::leave", cursor.set_to_left_ptr)
end

for _, c in ipairs({"left_ptr", "hand1"}) do
	cursor["set_to_"..c] = function()
		root.cursor(c)
		local wibox = mouse.current_wibox
		if wibox then wibox.cursor = c end
	end
end


return cursor
