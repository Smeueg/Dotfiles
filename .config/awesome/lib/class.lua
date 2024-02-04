--------------------------------------------------------------------------------
--- A Library To Make Classes Easier
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local class = setmetatable(
	{},
	{
		__call = function()
			local obj = {}
			obj.mt = { __index = obj }
			return obj
		end
	}
)
return class
