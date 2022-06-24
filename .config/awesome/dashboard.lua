local beautiful = require("beautiful")
local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")
local gears = require("gears")
local cairo = require("lgi").cairo
local module = {}
local border_color = beautiful.border_focus
local border_width = beautiful.border_width

local geometry = awful.screen.focused().geometry
local width = geometry.width / 3

local function separator()
	return {
		widget = wibox.widget.separator,
		orientation = "horizontal",
		color = beautiful.titlebar_bg_focus,
		forced_width = width,
		forced_height = 3,
		thickness = 3
	}
end

local widgets = {
	function() -- Hostname
		local widget = {}
		local f = io.open("/etc/hostname")
		local hostname = f:read("*l")
		f:close()
		local user = string.format(
			"<span foreground='%s'>%s</span>",
			border_color,
			os.getenv("USER")
		)
		hostname = string.format(
			"<span foreground='%s'>%s</span>",
			beautiful.fg_focus,
			hostname
		)
		return {
			widget = wibox.widget.textbox,
			valign = "center",
			align = "center",
			markup = string.format(
				"<span font='14' weight='bold'>%s@%s</span>",
				user,
				hostname
			)
		}
	end,
	separator,
	function() -- Info
		local widget = {
			layout = wibox.layout.grid,
			homogeneous = false,
			forced_num_cols = 2,
			spacing = 2
		}

		local function add(label, value)
			table.insert(
				widget,
				{
					widget = wibox.widget.textbox,
					align = "right",
					markup = string.format(
						"<span weight='bold' foreground='%s'>%s: </span>",
						beautiful.border_focus,
							label
					)
				}
			)
			table.insert(
				widget,
				{
					widget = wibox.widget.textbox,
					text = " " .. value
				}
			)
		end

		do -- resolution
			add("Resolution", geometry.width .. "x" .. geometry.height)
		end

		do -- OS
			local f = io.open("/etc/os-release")
			local OS = f:read("*l"):match("\"(.*)\"")
			f:close()
			add("OS", OS)
		end

		if gears.filesystem.file_readable("/proc/uptime") then -- Uptime
			local f = io.open("/proc/uptime", "r")
			local uptime = f:read("*l"):match("^(%d*).")
			f:close()
			local hour = math.floor(uptime / 3600)
			local minute = (math.floor(uptime / 60) - hour * 60)
			add("Uptime", hour .. " hours, " .. minute .. " minutes")
		end

		if gears.filesystem.file_readable("/proc/version") then -- Kernel
			local f = io.open("/proc/version")
			add("Kernel", f:read("*l"):match("^%w* %w* ([^ ]*)"))
			f:close()
		end

		if gears.filesystem.file_readable("/proc/version") then -- Memory
			local mem_total, mem_used
			local f = io.open("/proc/meminfo")
			local meminfo = f:read("*a")
			f:close()
			mem_total = meminfo:match("MemTotal:%s*([0-9]+)")
			mem_used = mem_total + meminfo:match("Shmem:%s*([0-9]+)")
			mem_used = mem_used - meminfo:match("MemFree:%s*([0-9]+)")
			mem_used = mem_used - meminfo:match("Buffers:%s*([0-9]+)")
			mem_used = mem_used - meminfo:match("Cached:%s*([0-9]+)")
			mem_used = mem_used - meminfo:match("SReclaimable:%s*([0-9]+)")
			mem_total = mem_total / 1024 ^ 2
			mem_used = mem_used / 1024 ^ 2
			add("Memory", string.format("%.2fGB / %.2fGB", mem_used, mem_total))
		end

		return {
			widget = wibox.container.place,
			widget
		}
	end,
	separator,
	function()
		local surface_create = cairo.ImageSurface.create
		local transform = gears.shape.transform
		local cairo_format = cairo.Format.ARGB32
		local pi = math.pi
		local cr
		local widget = {
			widget = wibox.container.background,
			layout = wibox.layout.grid,
			homogeneous = true,
			expand = false,
			forced_num_rows = 1,
			spacing = 10
		}

		local function notify(text)
			naughty.notify { text = text }
		end

		local opts = {
			{
				icon = surface_create(cairo_format, 20, 20),
				func = function()
					notify("Suspending System")
					awful.spawn("systemctl suspend")
				end
			},
			{
				icon = surface_create(cairo_format, 20, 20),
				func = function()
					notify("Powering Off System")
					awful.spawn("systemctl poweroff")
				end
			},
			{
				icon = surface_create(cairo_format, 20, 20),
				func = function()
					notify("Rebooting System")
					awful.spawn("systemctl reboot")
				end
			}
		}

		-- Suspend
		cr = cairo.Context(opts[1].icon)
		cr:set_source(gears.color(beautiful.wibar_selected_tag))
		transform(gears.shape.rounded_bar):translate(3, 3)(cr, 14, 14)
		cr:fill()
		cr:set_operator(cr, cairo.Operator.clear)
		transform(gears.shape.rounded_bar):translate(9, 1)(cr, 11, 11)
		cr:fill()
		-- Shutdown
		cr = cairo.Context(opts[2].icon)
		cr:set_source(gears.color(beautiful.wibar_selected_tag))
		transform(gears.shape.rounded_bar):translate(9, 3)(cr, 2, 7)
		transform(gears.shape.arc)
			:translate(3, 3)(cr, 14, 14, 2, -0.3 * pi, 1.3 * pi, true, true)
		cr:fill()
		-- Reboot
		cr = cairo.Context(opts[3].icon)
		cr:set_source(gears.color(beautiful.wibar_selected_tag))
		gears.shape.transform(gears.shape.arc)
			:translate(3, 3)(cr, 14, 14, 2, -0.1 * pi, 1.5 * pi, true, true)
		gears.shape.transform(gears.shape.isosceles_triangle)
			:translate(10, 3)
			:rotate_at(11, 2, -pi / 8)(cr, 7, 7)
		cr:fill()
		cr = nil

		for _, opt in ipairs(opts) do
			table.insert(
				widget,
				{
					widget = wibox.container.background,
					shape = gears.shape.rounded_rect,
					id = "option",
					func = opt.func,
					{
						widget = wibox.container.margin,
						margins = 5,
						{
							widget = wibox.widget.imagebox,
							image = opt.icon,
							forced_height = 30,
							forced_width = 30
						}
					}
				}
			)
		end
		widget.forced_num_cols = #opts
		widget = {
			widget = wibox.container.place,
			widget
		}
		return widget
	end
}

local function options_update(opts, chosen)
	for _, opt in ipairs(opts) do opt.bg = nil end
	opts[chosen].bg = beautiful.tasklist_bg_focus
end

local function options_press(opts, chosen)
	opts[chosen].func()
	module.toggle()
end

local function options_choose_next(opts)
	if module.chosen == #opts then return end
	module.chosen = module.chosen + 1
	options_update(opts, module.chosen)
end

local function options_choose_prev(opts)
	if module.chosen == 1 then return end
	module.chosen = module.chosen - 1
	options_update(opts, module.chosen)
end

function module.toggle()
	if module.popup ~= nil then
		module.keygrabber:stop()
		module.popup.visible = false
		module.popup = nil
		return
	end
	module.chosen = 1
	local popup_widget = {
		layout = wibox.layout.grid,
		homogeneous = false,
		spacing = 20,
		forced_num_cols = 1
	}

	for i, func in ipairs(widgets) do
		popup_widget[i] = func()
	end

	module.popup = awful.popup {
		placement = awful.placement.centered,
		border_width = border_width,
		border_color = border_color,
		shape = gears.shape.rounded_rect,
		ontop = true,
		widget = {
			widget = wibox.container.margin,
			margins = 20,
			popup_widget
		}
	}

	local opts = module.popup.widget:get_children_by_id("option")
	options_update(opts, module.chosen)
	for i, _ in ipairs(opts) do
		opts[i]:connect_signal("mouse::enter", function()
				module.chosen = i
				options_update(opts, i)
		end)
		opts[i]:connect_signal("button::press", function()
				options_press(opts, i)
		end)
	end

	-- Start keygrabber
	module.keygrabber = awful.keygrabber {
		autostart = true,
		keybindings = {
			{{}, "l", function() options_choose_next(opts) end},
			{{}, "h", function() options_choose_prev(opts) end},
			{{"Control"}, "n", function() options_choose_next(opts) end},
			{{"Control"}, "p", function() options_choose_prev(opts) end},
			{{"Control"}, "g", module.toggle},
			{{}, "Escape", module.toggle},
			{{}, "Return", function()
					options_press(opts, module.chosen)
			end},
			{{"Control"}, "m", function()
					options_press(opts, module.chosen)
			end}
		}
	}
end

do -- Widget for the wibar
	local icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
	local transform = gears.shape.transform
	local rectangle = gears.shape.rectangle
	local cr = cairo.Context(icon)
	cr:set_source(gears.color(beautiful.titlebar_bg_focus))
	transform(rectangle)
		:translate(4, 4)(cr, 12, 2)
	transform(rectangle)
		:translate(4, 9)(cr, 12, 2)
	transform(rectangle)
		:translate(4, 14)(cr, 12, 2)
	cr:fill()
	cr = nil

	local buttons = gears.table.join(awful.button({}, 1, module.toggle))

	module.widget = {
			widget = wibox.container.background,
			bg = beautiful.tasklist_bg_focus,
			shape = gears.shape.rounded_rect,
			buttons = buttons,
			{
				widget = wibox.widget.imagebox,
				image = icon
			}

	}
end

return module
