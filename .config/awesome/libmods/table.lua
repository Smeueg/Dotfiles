--- Run a function on all the elements of a table
---@param fn function
function table.map(t, fn)
	for _, v in ipairs(t) do
		fn(v)
	end
end
