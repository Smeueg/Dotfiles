local cairo = require("lgi").cairo

function cairo.CreateImage(body, size)
	local tmp = {}

	if not size then -- default value for `size`
		size = { 20, 20 }
	end

	tmp.image = cairo.ImageSurface.create(
		cairo.Format.ARGB32,
		size[1],
		size[2]
	)

	body(cairo.Context(tmp.image))
	return tmp.image
end

function cairo.surface_to_rgba(surface)
	local _, r, g, b = surface:get_rgba()
	r = math.floor(r * 255 + 0.5)
	g = math.floor(g * 255 + 0.5)
	b = math.floor(b * 255 + 0.5)
	return string.format("#%02X%02X%02X", r, g, b)
end
