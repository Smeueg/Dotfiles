--------------------------------------------------------------------------------
--- Extra functions for awful.layout
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local awful = require("awful")
local notify = require("naughty").notify

--- Set the layout for every tag
---@param layout awful.layout
function awful.layout.set_all(layout)
	for _, t in ipairs(root.tags()) do
		awful.layout.set(layout, t)
	end
end

--- Increase the master-width-factor for all tags
---@param factor number
function awful.layout.incmwf_all(factor)
	if awful.layout.get() ~= awful.layout.suit.tile.right then return end
	for _, t in ipairs(root.tags()) do awful.tag.incmwfact(factor, t) end
end

--- Increment the number of master windows for all tags
---@param n number
function awful.layout.incnmaster(n)
	if awful.layout.get() ~= awful.layout.suit.tile.right then return end
	if root.tags()[1].master_count == 1 and n < 0 then return end
	for _, t in ipairs(root.tags()) do awful.tag.incnmaster(n, t, true) end
	notify {
		title = "Current Master Count",
		text = tostring(root.tags()[1].master_count),
		position = "top_left",
		timeout = 2
	}
end
