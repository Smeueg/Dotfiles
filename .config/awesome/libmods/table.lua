function table.map(t, fn)
	for _, v in ipairs(t) do
		fn(v)
	end
end
