local gears = require("gears")
local notify = require("naughty").notify
local module = {}

function module.fade_in(widget, duration, callback_done)
	local step = 100
	if widget.animation_timer then widget.animation_timer:stop() end
	widget.animation_value = widget.animation_value or 0
	widget.animation_timer = gears.timer {
		timeout = duration / step,
		autostart = true,
		call_now = true,
		callback = function()
			widget.animation_value = widget.animation_value + 1
			widget.opacity = widget.animation_value / step
			if widget.animation_value == step then
				if callback_done then callback_done() end
				widget.animation_timer:stop()
			end
		end
	}
end

function module.fade_out(widget, duration, callback_done)
	local step = 100
	if widget.animation_timer then widget.animation_timer:stop() end
	widget.animation_value = widget.animation_value or step
	widget.animation_timer = gears.timer {
		timeout = duration / step,
		autostart = true,
		call_now = true,
		callback = function()
			widget.animation_value = widget.animation_value - 1
			widget.opacity = widget.animation_value / step
			if widget.animation_value == 0 then
				if callback_done then callback_done() end
				widget.animation_timer:stop()
			end
		end
	}
end

function module.animate(widget, duration, callback)
	local step = 100
	widget.animation_value = widget.animation_value or 0
	widget.animation_timer = gears.timer {
		timeout = duration / step,
		autostart = true,
		call_now = true,
		callback = function() callback(widget.animation_value) end
	}
end

function module.color_transition(widget, id, duration, hex_start, hex_end, callback)
	if not widget.animation then
		widget.animation = {}
	end

	if widget.animation[id] then
		widget.animation[id].timer:stop()
	else
		widget.animation[id] = {}
	end

	local step = 10
	local r_start = tonumber("0x"..string.sub(hex_start, 2, 3))
	local g_start = tonumber("0x"..string.sub(hex_start, 4, 5))
	local b_start = tonumber("0x"..string.sub(hex_start, 6, 7))
	local r_end = tonumber("0x"..string.sub(hex_end, 2, 3))
	local g_end = tonumber("0x"..string.sub(hex_end, 4, 5))
	local b_end = tonumber("0x"..string.sub(hex_end, 6, 7))
	local r_step = math.floor((r_end - r_start) / step)
	local g_step = math.floor((g_end - g_start) / step)
	local b_step = math.floor((b_end - b_start) / step)
	local colors = {}
	for i=1, step do
		r_start = r_start + r_step
		g_start = g_start + g_step
		b_start = b_start + b_step
		colors[i] = string.format("#%02x%02x%02x", r_start, g_start, b_start)
	end
	colors[step + 1] = hex_end
	widget.animation[id].value = widget.animation[id].value or 0
	widget.animation[id].timer = gears.timer {
		timeout = duration / step,
		autostart = true,
		call_now = true,
		callback = function()
			widget.animation[id].value = widget.animation[id].value + 1
			local color = colors[widget.animation[id].value]
			if color then
				callback(color)
			else
				widget.animation[id].timer:stop()
				widget.animation[id].value = nil
			end
		end
	}
end

return module
