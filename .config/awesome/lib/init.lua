local lib = {}

function lib.enum(enums)
	local enum = {}
	for _, v in ipairs(enums) do
		enum[v] = {}
	end
end

return lib
