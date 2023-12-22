local function enum(keys)
	local Enum = {}
	for _, key in ipairs(keys) do
		Enum[key] = {}
	end
	return Enum
end

return enum
