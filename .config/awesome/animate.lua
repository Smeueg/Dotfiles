local gears = require("gears")
local notify = require("naughty").notify
local module = {}
module.animations = {}

local function animation_prepare(id)
	if module.animations[id] then
		module.animations[id].timer:stop()
	else
		module.animations[id] = {}
	end
	module.animations[id].value = 0
end

function module.simple(id, duration, n_start, n_end, callback)
	animation_prepare(id)
	local steps = 10
	local step = (n_end - n_start) / steps
	local value = {}
	for i=1, steps - 1 do value[i] = n_start + step * i end
	value[steps] = n_end
	module.animations[id].timer = gears.timer {
		timeout = duration / steps,
		autostart = true,
		call_now = true,
		callback = function()
			module.animations[id].value = module.animations[id].value + 1
			local v = value[module.animations[id].value]
			if v then
				callback(v)
			else
				module.animations[id].timer:stop()
				module.animations[id] = nil
			end
		end
	}
end

function module.color(id, duration, hex_start, hex_end, callback)
	animation_prepare(id)
	local steps = 10
	local r_start = tonumber("0x"..string.sub(hex_start, 2, 3))
	local g_start = tonumber("0x"..string.sub(hex_start, 4, 5))
	local b_start = tonumber("0x"..string.sub(hex_start, 6, 7))
	local r_end = tonumber("0x"..string.sub(hex_end, 2, 3))
	local g_end = tonumber("0x"..string.sub(hex_end, 4, 5))
	local b_end = tonumber("0x"..string.sub(hex_end, 6, 7))
	local r_step = math.floor((r_end - r_start) / steps)
	local g_step = math.floor((g_end - g_start) / steps)
	local b_step = math.floor((b_end - b_start) / steps)
	local colors = {}
	for i=1, steps - 1 do
		r_start = r_start + r_step
		g_start = g_start + g_step
		b_start = b_start + b_step
		colors[i] = string.format("#%02x%02x%02x", r_start, g_start, b_start)
	end
	colors[steps] = hex_end
	module.animations[id].timer = gears.timer {
		timeout = duration / steps,
		autostart = true,
		call_now = true,
		callback = function()
			module.animations[id].value = module.animations[id].value + 1
			local color = colors[module.animations[id].value]
			if color then
				callback(color)
			else
				module.animations[id].timer:stop()
				module.animations[id] = nil
			end
		end
	}
end

return module
