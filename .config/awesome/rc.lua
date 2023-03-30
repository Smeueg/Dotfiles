--[[
	Smeueg's AwesomeWM Configuration File

	REQUIRES:
	- pactl (Pulseaudio)
	- nmcli (NetworkManager)

	TODO:
	- Cleanup
	- Add USEFULL comments
	- Add animations
--]]

-- Import Libraries
pcall(require, "luarocks.loader") -- Make sure LuaRocks packages is loaded
require("awful.autofocus")
local beautiful = require("beautiful")
local apply_dpi = beautiful.xresources.apply_dpi
local naughty = require("naughty")
local notify = naughty.notify
local gears = require("gears")
local shapes = gears.shape
local awful = require("awful")
local wibox = require("wibox")
local lgi = require("lgi")
local cairo = lgi.cairo
local Gio = lgi.Gio
local pi = math.pi

-- Error Handling --
do
	local in_error = false
	awesome.connect_signal(
		"debug::error",
		function(err)
			if in_error then return end
			in_error = true
			notify {
				title = "Error",
				text = tostring(err),
				fg = "#FABD2F",
				timeout = 0
			}
			in_error = false
		end
	)
end


-- Theme (Gruvbox) --
beautiful.init {
	wallpaper = "#282828",
	-- Default
	font = "JetBrainsMono Nerd Font Mono 11",
	bg_normal = "#32302f",
	fg_normal = "#EBDBB2",
	-- Windows
	border_width = apply_dpi(2),
	border_focus = "#FE8019",
	border_normal = "#282828",
	useless_gap = apply_dpi(10),
	-- Titlebar
	titlebar_bg = "#32302f",
	titlebar_btn_max = "#B8BB26",
	titlebar_btn_min = "#FABD2F",
	titlebar_btn_close = "#D65D0E",
	-- Wibar
	wibar_height = apply_dpi(50),
	wibar_icon_color = "#FABD2F",
	wibar_position = "top",
	-- Calendar
	calendar_fg_normal = "#43413F",
	-- Notifications
	notification_border_color = "#FE8019",
	-- Tags/Taglist
	tag_amount = 3,
	taglist_fg_focus = "#FABD2F",
	taglist_fg_normal = "#504945",
	-- Tasklist
	tasklist_bg_focus = "#00000030",
	-- Launcher
	launcher_app_margin = apply_dpi(10),
	launcher_limit = 10
}

naughty.config.spacing = apply_dpi(5)



-- Custom Functions --
local utils = {
	check_cmd = function(cmd)
		for dir in string.gmatch(os.getenv("PATH"), "([^:]+)") do
			if gears.filesystem.file_executable(dir .. "/" .. cmd) then
				return true
			end
		end
		return false
	end,
	layout_set = function(layout)
		local layouts = awful.layout.suit
		local gap = 0
		local border = 0
		local show_titlebar = false

		if layout == layouts.tile.right then
			gap = beautiful.useless_gap
			border = beautiful.border_width
		elseif layout == layouts.floating then
			show_titlebar = true
		end

		for _, t in ipairs(root.tags()) do
			awful.layout.set(layout, t)
			t.useless_gap = gap
		end

		for _, c in ipairs(client.get()) do
			c.border_width = border
			if show_titlebar then
				awful.titlebar.show(c)
			elseif not c.floating then
				awful.titlebar.hide(c)
			end
		end
	end,
	layout_incmwfact = function(factor)
		if awful.layout.get() ~= awful.layout.suit.tile.right then return end
		for _, t in ipairs(root.tags()) do awful.tag.incmwfact(factor, t) end
	end,
	launch = function(name, cmd)
		notify { title = "Launching Application", text = name }
		awful.spawn(cmd)
	end,
	style = function(str, markup)
		local str_style = ""
		local function check_and_add(attr)
			if markup[attr] then
				str_style = string.format(
					"%s %s='%s'",
					str_style,
					attr,
					markup[attr]
				)
			end
		end
		local styles = {
			"foreground",
			"background",
			"font_weight",
			"font_size"
		}
		for _, v in ipairs(styles) do check_and_add(v) end
		return string.format("<span%s>%s</span>", str_style, str)
	end,
	font_size = function(size)
		return string.format("%s %i", beautiful.font:match("(.*) [0-9]+"), size)
	end
}

function string:upper_first()
	return self:sub(1, 1):upper() .. self:sub(2)
end

function string.style_for_pango(text, style)
	local options = ""
	for key, v in pairs(style) do
		options = options .. string.format(" %s='%s'", key, v)
	end
	return string.format("<span%s>%s</span>", options, text)
end

function awful.widget.border_wrapper(w, disabled)
	disabled = disabled or {}
	local function border(orientation, position)
		if disabled[position] then
			return { widget = wibox.container.background }
		end

		local attr = orientation == "vertical" and "width" or "height"
		return {
			widget = wibox.container.background,
			bg = beautiful.border_focus,
			{
				widget = wibox.container.constraint,
				["forced_"..attr] = beautiful.border_width,
			}
		}
	end

	return {
		widget = wibox.container.background,
		{
			layout = wibox.layout.align.vertical,
			border(nil, "top"),
			{
				layout = wibox.layout.align.horizontal,
				border("vertical", "left"),
				w,
				border("vertical", "right"),
			},
			border(nil, "bottom")
		}
	}
end

function root.execute_keybinding(modifiers, key)
	local conversion = {
		Mod4 = "Super_L",
		Control = "Control_L",
		Shift = "Shift_L",
		Mod1 = "Alt_L",
		Mod4 = "Super_R",
		Control = "Control_R",
		Shift = "Shift_R",
		Mod1 = "Alt_R"
	}

	for _, mod in ipairs(modifiers or {}) do
		root.fake_input("key_press", conversion[mod])
	end

	root.fake_input("key_press", key)
	root.fake_input("key_release", key)

	for _, mod in ipairs(modifiers or {}) do
		root.fake_input("key_release", conversion[mod])
	end
end

function cairo.CreateImage(body, size)
	local tmp = {}
	local size = size
	if not size then
		size = { 20, 20 }
	end
	tmp.image = cairo.ImageSurface.create(cairo.Format.ARGB32, size[1], size[2])
	tmp.cr = cairo.Context(tmp.image)
	if type(body) == "table" then
		for _, v in ipairs(body) do
			v(tmp.cr)
		end
	elseif type(body) == "function" then
		body(tmp.cr)
	end
	return tmp.image
end

function cairo.get_rgb_as_hex(pattern)
	local _, r, g, b = pattern:get_rgba()
	r = math.floor(r * 255 + 0.5)
	g = math.floor(g * 255 + 0.5)
	b = math.floor(b * 255 + 0.5)
	return string.format("#%02X%02X%02X", r, g, b)
end

function gears.filesystem.find_executable(executable)
	local path
	for dir in string.gmatch(os.getenv("PATH"), "([^:]+)") do
		path = dir .. "/" .. executable
		if gears.filesystem.file_executable(path) then return path end
	end
end

function awful.spawn.run_if_installed(cmds)
	for _, cmd in ipairs(cmds) do
		local bin = cmd:match("^[^ ]+")
		if gears.filesystem.find_executable(bin) then
			awful.spawn(cmd, false)
		else
			notify {
				title = "Warning",
				text = string.format("'%s' isn't installed", bin)
			}
		end
	end
end


-- Custom Shapes --
function gears.shape.rounded_rect_auto(cr, width, height)
	gears.shape.rounded_rect(cr, width, height, apply_dpi(5))
end


-- Custom Widgets --
function awful.widget.date()
	return {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		bg = "#00000030",
		{
			widget = wibox.widget.textclock,
			format = " %a - %d/%m/%y  %H:%M "
		}
	}
end

do -- awful.widget.volume
	local icons = {
		unmute = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				shapes.transform(shapes.rectangle)
					:translate(6, 8)(cr, 4, 4)
				shapes.transform(shapes.isosceles_triangle)
					:rotate_at(0, 0, pi / -2)
					:translate(-14, 6)(cr, 8, 4)
				shapes.transform(shapes.arc)
					:translate(0, 4)(cr, 12, 12, 1, pi / -7, pi / 7, true, true)
				shapes.transform(shapes.arc)
					:translate(0, 3)(cr, 14, 14, 1, pi / -6, pi / 6, true, true)
				shapes.transform(shapes.arc)
					:translate(0, 2)(cr, 16, 16, 1, pi / -5, pi / 5, true, true)
				cr:fill()
		end),
		mute = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				shapes.transform(shapes.rectangle)
					:translate(6, 8)(cr, 4, 4)
				shapes.transform(shapes.isosceles_triangle)
					:rotate_at(0, 0, pi / -2)
					:translate(-14, 6)(cr, 8, 4)
				shapes.transform(shapes.cross)
					:rotate_at(0, 0, pi / 4)
					:translate(13.5, -5)(cr, 6, 6, 1)
				cr:fill()
		end)
	}

	local widget = wibox.widget {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		bg = "#00000030",
		buttons = gears.table.join(
			awful.button({}, 1, function()
					awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
			end),
			awful.button({}, 4, function()
					awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +1%", false)
			end),
			awful.button({}, 5, function()
					awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ -1%", false)
			end)
		),
		{
			widget = wibox.container.margin,
			layout = wibox.layout.fixed.horizontal,
			left = apply_dpi(5),
			right = apply_dpi(5),
			{
				widget = wibox.container.background,
				{
					widget = wibox.widget.imagebox,
					id = "icon",
				}
			},
			{
				widget = wibox.widget.textbox,
				id = "vol"
			},
			{
				widget = wibox.widget.textbox,
				text = " "
			}
		}
	}

	local function update()
		local vol_regex = ""
		for _=1, 6 do vol_regex = vol_regex .. "[^\n]*\n" end
		local w_vol = widget:get_children_by_id("vol")[1]
		local w_icon = widget:get_children_by_id("icon")[1]
		awful.spawn.easy_async(
			"pactl info",
			function(stdout)
				local sink = stdout:match("Default Sink: ([^\n]+)")
				sink = sink:gsub("-", "[-]")
				awful.spawn.easy_async(
					"pactl list sinks",
					function(stdout)
						local regex = "Name: " .. sink .. vol_regex
						local volume
						local image
						if stdout:match(regex .. "%s*Mute: no") then
							image = icons.unmute
						else
							image = icons.mute
						end
						regex = regex .. "[^\n]*\n[^/]+/ *(%d+%%) */"
						volume = stdout:match(regex)
						widget:get_children_by_id("vol")[1].text = volume
						widget:get_children_by_id("icon")[1].image = image
					end
				)
			end
		)
	end

	local function watch()
		update()
		awful.spawn.with_line_callback(
			"pactl subscribe",
			{
				stdout = function(line)
					if not line:match("'change' on sink ") then return end
					update()
				end,
				exit = spawn_watcher
			}
		)
	end

	awful.spawn.with_line_callback(
		"ps -C pactl -o pid=,cmd=",
		{
			stdout = function(line)
				local pid = line:match("(%d+)%s+pactl subscribe")
				if pid then
					awful.spawn("kill -9 " .. pid, false)
				end
			end,
			output_done = watch
		}
	)

	function awful.widget.volume()
		return widget
	end
end

function awful.widget.taglist_styled(s)
	local function update(self, t)
		local widget = self:get_children_by_id("icon")[1]
		if t.selected then
			widget.shape_border_color = beautiful.taglist_fg_focus
		else
			widget.shape_border_color = beautiful.taglist_fg_normal
		end
		widget.bg = next(t:clients()) and widget.shape_border_color or nil
	end

	return awful.widget.taglist {
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = gears.table.join(
			awful.button({}, 1, function(t) t:view_only() end),
			awful.button({}, 3, function(t)
						local c = client.focus
						if c then c:move_to_tag(t) end
			end),
			awful.button({}, 4, function(t) awful.tag.viewidx(-1) end),
			awful.button({}, 5, function(t) awful.tag.viewidx(1) end)
		),
		layout = {
			layout = wibox.layout.fixed.horizontal,
			spacing = apply_dpi(10)
		},
		widget_template = {
			widget = wibox.container.margin,
			margins = apply_dpi(1),
			{
				id = "icon",
				widget = wibox.container.background,
				shape = gears.shape.circle,
				shape_border_width = apply_dpi(2),
				forced_height = apply_dpi(15),
				forced_width = apply_dpi(15),
				{ widget = wibox.widget {} }
			},
			create_callback = update,
			update_callback = update
		}
	}
end

function awful.widget.tasklist_styled(s)
	return awful.widget.tasklist {
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		style = { shape = gears.shape.rounded_rect_auto },
		buttons = gears.table.join(
			awful.button({}, 1, function(c)
					if c == client.focus then
						c.minimized = true
					else
						c:emit_signal(
							"request::activate",
							"tasklist",
							{raise = true}
						)
					end
			end),
			awful.button({ }, 4, function() awful.client.focus.byidx(1) end),
			awful.button({ }, 5, function() awful.client.focus.byidx(-1) end)
		),
		widget_template = {
			widget = wibox.container.background,
			id = "background_role",
			{
				widget = wibox.container.margin,
				margins = apply_dpi(5),
				{
					widget = wibox.widget.imagebox,
					id = "icon_role",
					forced_height = apply_dpi(20),
					forced_width = apply_dpi(20)
				}
			}
		}
	}
end

do -- awful.widget.network
	local icons = {
		offline = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				local c_start = (1.5 - 1/5) * pi
				local c_end = (1.5 + 1/5) * pi
				shapes.transform(shapes.pie)
					:translate(0, 5)(cr, 20, 20, c_start, c_end)
				cr:set_line_width(1)
				cr:stroke()
		end),
		wifi = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				local c_start = (1.5 - 1/5) * pi
				local c_end = (1.5 + 1/5) * pi
				shapes.transform(shapes.pie)
					:translate(0, 5)(cr, 20, 20, c_start, c_end)
				cr:set_line_width(1)
				cr:fill()
		end),
		ethernet = cairo.CreateImage(function(cr)
				cr:rectangle(8, 4, 3, 3)
				cr:rectangle(5, 12, 3, 3)
				cr:rectangle(11, 12, 3, 3)
				cr:rectangle(9, 7, 1, 3)
				cr:rectangle(6, 9, 1, 3)
				cr:rectangle(12, 9, 1, 3)
				cr:rectangle(6, 9, 6, 1)
				cr:fill()
		end)
	}

	local widget = wibox.widget {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		bg = "#00000030",
		{
			widget = wibox.container.background,
			layout = wibox.layout.fixed.horizontal,
			{
				widget = wibox.widget.imagebox,
				id = "icon",
			},
			{
				widget = wibox.widget.textbox,
				id = "net"
			},
			{
				widget = wibox.widget.textbox,
				text = " "
			}
		}
	}

	gears.timer {
		timeout = 10,
		autostart = true,
		call_now = true,
		callback = function()
			awful.spawn.easy_async(
				"nmcli -t -f name,type connection show --active",
				function(stdout)
					local icon = widget:get_children_by_id("icon")[1]
					local net = widget:get_children_by_id("net")[1]
					if not stdout then
						icon.image = icons.offline
						net.text = "Offline"
						return
					end
					local net_name, net_type = stdout:match("([^:]+):([^:]+)")
					if net_type and net_type:match("-wireless") then
						icon.image = icons.wifi
					else
						icon.image = icons.ethernet
					end
					net.text = net_name or ""
				end
			)
		end
	}

	function awful.widget.network() return widget end
end

do -- awful.widget.screenshot & awful.widget.screenshot.popup
	local templates = {
		option = function(text, callback)
			return {
				widget = wibox.container.background,
				callback = callback,
				id = "option",
				{
					widget = wibox.container.margin,
					margins = apply_dpi(10),
					{
						widget = wibox.widget.textbox,
						markup = string.style_for_pango(text, { font = 12 }),
						align = "center"
					}
				}
			}
		end,
		path = function()
			return os.date("'/tmp/Screenshot %d-%m-%Y %X.png'")
		end
	}

	local function place(p)
		awful.placement.next_to(
			p,
			{
				preferred_positions = "bottom",
				preferred_anchors = "middle",
				geometry = awful.screen.focused().wibar
			}
		)
		p.y = p.y - beautiful.border_width
	end

	local popup = awful.popup {
		placement = place,
		ontop = true,
		visible = false,
		widget = awful.widget.border_wrapper({
				widget = wibox.container.margin,
				margins = apply_dpi(10),
				{
					layout = wibox.layout.fixed.vertical,
					spacing = apply_dpi(10),
					{
						widget = wibox.widget.textbox,
						align = "center",
						markup = string.style_for_pango("Screenshot", {
								font = 12,
								fgcolor = beautiful.border_focus,
								weight = "bold"
						})
					},
					{
						layout = wibox.layout.grid,
						orientation = "horizontal",
						spacing = apply_dpi(5),
						templates.option("Full", function()
								local path = templates.path()
								awful.spawn.easy_async_with_shell(
									"sleep 0.05; import -window root " .. path,
									function(_, _, _, exit_code)
										if exit_code ~= 0 then return end
										notify {
											title = "Took Screenshot",
											text = path
										}
									end
								)
						end),
						templates.option("Partial", function()
								local path = templates.path()
								awful.spawn.easy_async_with_shell(
									"sleep 0.05; import " .. path,
									function(_, _, _, exit_code)
										if exit_code ~= 0 then return end
										notify {
											title = "Took Screenshot",
											text = path
										}
									end
								)
						end)
					}
				}
		}, { top = true })
	}

	popup.options = popup.widget:get_children_by_id("option")

	function popup.update()
		for i, w in ipairs(popup.options) do
			if i == popup.chosen then
				w.bg = "#00000030"
			else
				w.bg = nil
			end
		end
	end

	function popup.prev()
		if popup.chosen > 1 then
			popup.chosen = popup.chosen - 1
		end
		popup.update()
	end

	function popup.next()
		if popup.chosen < #popup.options then
			popup.chosen = popup.chosen + 1
		end
		popup.update()
	end

	function popup.press()
		popup.options[popup.chosen].callback()
		popup.toggle()
	end

	function popup.toggle()
		if popup.visible then
			popup.visible = false
			popup.keygrabber:stop()
			return
		end
		popup.chosen = 1
		popup.update()
		place(popup)
		popup.visible = true
		popup.keygrabber:start()
	end

	popup.widget:buttons(awful.button({}, 1, popup.press))

	for i, w in ipairs(popup.options) do
		w:connect_signal("mouse::enter", function()
				popup.chosen = i
				popup.update()
		end)
	end

	popup.keygrabber = awful.keygrabber {
		keybindings = {
			{ {}, "Left", popup.prev },
			{ {}, "Right", popup.next },
			{ {}, "h", popup.prev },
			{ {}, "l", popup.next },
			{ {}, "Escape", popup.toggle },
			{ {}, "Return", popup.press },
			{ {"Control"}, "p", popup.prev },
			{ {"Control"}, "n", popup.next },
			{ {"Control"}, "g", popup.toggle },
			{ {"Control"}, "j", popup.press }
		}
	}

	local widget = {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		bg = "#00000030",
		buttons = awful.button({}, 1, popup.toggle),
		{
			widget = wibox.widget.imagebox,
			image = cairo.CreateImage(function(cr)
					cr:set_source(gears.color(beautiful.wibar_icon_color))
					cr:rectangle(4, 4, 4, 2)
					cr:rectangle(4, 4, 2, 4)
					cr:rectangle(4, 14, 4, 2)
					cr:rectangle(4, 12, 2, 4)
					cr:rectangle(12, 4, 4, 2)
					cr:rectangle(14, 4, 2, 4)
					cr:rectangle(12, 14, 4, 2)
					cr:rectangle(14, 12, 2, 4)
					cr:fill()
			end)
		}
	}

	awful.widget.screenshot = setmetatable(
		{ popup = popup.toggle },
		{ __call = function() return widget end }
	)
end

do -- awful.widget.layout
	local icons = {
		tile = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				cr:rectangle(4, 4, 5, 12)
				cr:rectangle(11, 4, 5, 5)
				cr:rectangle(11, 11, 5, 5)
				cr:fill()
		end),
		max = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				cr:rectangle(4, 4, 12, 12)
				cr:fill()
		end),
		float = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				cr:rectangle(4, 4, 8, 8)
				cr:rectangle(13, 8, 3, 8)
				cr:rectangle(8, 13, 8, 3)
				cr:fill()
		end)
	}

	local widget = wibox.widget {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		bg = "#00000030",
		buttons = awful.button({}, 1, function(self)
				local layout = root.tags()[1].layout
				if layout == awful.layout.suit.tile.right then
					utils.layout_set(awful.layout.suit.max)
				elseif layout == awful.layout.suit.max then
					utils.layout_set(awful.layout.suit.floating)
				else
					utils.layout_set(awful.layout.suit.tile.right)
				end
		end),
		{ widget = wibox.widget.imagebox }
	}

	local function update(t)
		if t.index > 1 then return end
		if t.layout == awful.layout.suit.tile.right then
			widget:get_children()[1].image = icons.tile
		elseif t.layout == awful.layout.suit.max then
			widget:get_children()[1].image = icons.max
		else
			widget:get_children()[1].image = icons.float
		end
	end

	tag.connect_signal("property::layout", update)

	function awful.widget.layout()
		return widget
	end
end

do -- awful.widget.dashboard & awful.widget.dashboard.popup
	local icons = {
		shutdown = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				shapes.transform(shapes.arc)
					:translate(3, 3)(cr, 14, 14, 2, -0.4*math.pi, -0.6*math.pi)
				shapes.transform(shapes.rounded_bar)
					:translate(9, 1)(cr, 2, 8)
				cr:fill()
		end),
		reboot = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				shapes.transform(shapes.arc)
					:translate(3, 3)(cr, 14, 14, 2, -0.3*math.pi, -0.7*math.pi)
				shapes.transform(shapes.isosceles_triangle)
					:translate(15, 2):rotate_at(15, 2, math.pi/2.75)(cr, 5, 5)
				cr:fill()
		end),
		suspend = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				shapes.transform(shapes.circle):translate(2, 2)(cr, 16, 16)
				cr:fill()
				cr:set_operator(cr, cairo.Operator.clear)
				shapes.transform(shapes.circle):translate(0, 0)(cr, 12, 12)
				cr:fill()
		end),
		launcher = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				shapes.transform(shapes.circle):translate(4, 4)(cr, 8, 8)
				cr:stroke()
				shapes.transform(shapes.rectangle)
					:translate(11, 10)
					:rotate_at(11, 10, math.pi/4)(cr, 6, 3)
				cr:fill()
		end),
		home = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.wibar_icon_color))
				cr:rectangle(4, 8, 12, 8)
				shapes.transform(shapes.isosceles_triangle)
					:translate(4, 4)(cr, 12, 4)
				cr:fill()
				cr:set_operator(cr, cairo.Operator.clear)
				shapes.transform(shapes.circle)
					:translate(7, 10)(cr, 6, 6)
				cr:rectangle(7, 13, 6, 6)
				cr:fill()
		end)
	}

	local templates = {
		header = function(img)
			return {
				widget = wibox.container.background,
				shape = gears.shape.rounded_rect_auto,
				{
					widget = wibox.container.margin,
					margins = apply_dpi(5),
					{
						widget = wibox.widget.imagebox,
						image = img,
						forced_height = apply_dpi(30),
						forced_width = apply_dpi(30)
					}
				}
			}
		end,
		container = function()
			return wibox.widget {
				layout = wibox.layout.grid,
				homogeneous = false
			}
		end,
		power_opt = function(img_id, callback)
			return {
				widget = wibox.container.background,
				callback = callback,
				id = "opt",
				shape = gears.shape.rounded_rect_auto,
				{
					widget = wibox.container.margin,
					margins = apply_dpi(10),
					{
						widget = wibox.container.place,
						halign = "center",
						valign = "center",
						{
							widget = wibox.widget.imagebox,
							image = icons[img_id],
							forced_height = apply_dpi(25),
							forced_width = apply_dpi(25),
						}
					}
				}
			}
		end,
		sys_cmd = function(cmd)
			if utils.check_cmd("systemctl") then
				return "systemctl " .. cmd
			end
			return "loginctl " .. cmd
		end
	}

	local function place(p)
		awful.placement.next_to(
			p,
			{
				preferred_positions = "bottom",
				preferred_anchors = "front",
				geometry = awful.screen.focused().wibar
			}
		)

		p.y = p.y - beautiful.border_width
		p.x = p.x - beautiful.border_width
	end

	local popup_power = awful.popup {
		placement = place,
		ontop = true,
		visible = false,
		widget = awful.widget.border_wrapper({
				widget = wibox.container.margin,
				margins = apply_dpi(10),
				{
					layout = wibox.layout.fixed.horizontal,
					templates.power_opt("suspend", function()
							notify { text = "Suspending System" }
							awful.spawn(templates.sys_cmd("suspend"))
					end),
					templates.power_opt("reboot", function()
							notify { text = "Rebooting System" }
							awful.spawn(templates.sys_cmd("reboot"))
					end),
					templates.power_opt("shutdown", function()
							notify { text = "Shutting System Down" }
							awful.spawn(templates.sys_cmd("poweroff"))
					end)
				}
		}, {top = true, left = true})
	}

	popup_power.options = popup_power.widget:get_children_by_id("opt")
	popup_power.chosen = 1

	function popup_power:style()
		for i, w in ipairs(self.options) do
			w.bg = (i == self.chosen) and "#00000030" or nil
		end
	end

	function popup_power:next()
		if self.chosen < #self.options then
			self.chosen = self.chosen + 1
		end
		self:style()
	end

	function popup_power:prev()
		if self.chosen > 1 then
			self.chosen = self.chosen - 1
		end
		self:style()
	end

	function popup_power:press()
		self.options[self.chosen].callback()
		self:toggle()
	end

	function popup_power:toggle()
		if self.visible then
			self.visible = false
			self.keygrabber:stop()
			return
		end
		self.chosen = 1
		self.visible = true
		self:style()
		self.keygrabber:start()
	end

	for i, w in ipairs(popup_power.options) do
		w:buttons(awful.button({}, 1, function() popup_power:press() end))
		w:connect_signal("mouse::enter", function()
				popup_power.chosen = i
				popup_power.style()
		end)
	end

	popup_power.keygrabber = awful.keygrabber {
		keybindings = {
			{ {}, "Left", function() popup_power:prev() end },
			{ {}, "Right", function() popup_power:next() end },
			{ {}, "Escape", function() popup_power:toggle() end },
			{ {}, "Return", function() popup_power:press() end },
			{ {}, "h", function() popup_power:prev() end },
			{ {}, "l", function() popup_power:next() end },
			{ { "Control" }, "p", function() popup_power:prev() end },
			{ { "Control" }, "n", function() popup_power:next() end },
			{ { "Control" }, "g", function() popup_power:toggle() end },
			{ { "Control" }, "j", function() popup_power:press() end }
		}
	}

	-- Launcher --
	local popup_launcher = awful.popup {
		placement = place,
		ontop = true,
		visible = false,
		widget = awful.widget.border_wrapper({
				widget = wibox.container.margin,
				margins = apply_dpi(10),
				{
					layout = wibox.layout.fixed.vertical,
					spacing = apply_dpi(10),
					{
						layout = wibox.layout.fixed.horizontal,
						spacing = apply_dpi(-10),
						{
							widget = wibox.container.margin,
							margins = apply_dpi(10),
							{
								widget = wibox.widget.imagebox,
								image = icons.launcher,
								forced_height = apply_dpi(20),
								forced_width = apply_dpi(20),
							}
						},
						{
							widget = wibox.container.background,
							fg = beautiful.fg_normal .. "80",
							{
								widget = wibox.container.margin,
								margins = apply_dpi(10),
								{
									widget = wibox.widget.textbox,
									font = utils.font_size(12),
									id = "prompt"
								}
							}
						}
					},
					{
						layout = wibox.layout.fixed.horizontal,
						{
							layout = wibox.layout.grid,
							id = "grid"
						},
						{
							widget = wibox.widget.imagebox,
							id = "scrollbar",
							forced_width = apply_dpi(10),
							resize = false
						}
					}
				}
		}, { top = true, left = true })
	}
	popup_launcher.limit = beautiful.launcher_limit or 10
	popup_launcher.entries = {}
	popup_launcher.grid = popup_launcher.widget:get_children_by_id("grid")[1]
	popup_launcher.scrollbar = popup_launcher.widget:get_children_by_id("scrollbar")[1]
	popup_launcher.prompt = popup_launcher.widget:get_children_by_id("prompt")[1]
	for _, e in ipairs(Gio.AppInfo.get_all()) do
		if e:should_show() then
			table.insert(popup_launcher.entries, {
					appinfo = e,
					name = e:get_name()
						:gsub("&", "&amp;")
						:gsub("<", "&lt;")
						:gsub("'", "&#39;")
			})
		end
	end

	function popup_launcher:style()
		local childrens = self.grid:get_children()
		local idx = self.chosen - self.scroll
		if childrens[idx - 1] then
			childrens[idx - 1].bg = nil
		end

		if childrens[idx] then
			childrens[idx].bg = "#00000030"
		end

		if childrens[idx + 1] then
			childrens[idx + 1].bg = nil
		end
	end

	function popup_launcher:filter(text)
		self.entries_filtered = {}
		for _, e in ipairs(self.entries) do
			if e.name:lower():match(text:lower()) then
				table.insert(self.entries_filtered, e)
			end
		end

		if self.chosen > #self.entries_filtered then
			self.chosen = #self.entries_filtered
		end

		self.grid:reset()
		for i, e in ipairs(self.entries_filtered) do
			if i >= self.scroll + 1 and i <= self.scroll + self.limit then
				self.grid:add(
					wibox.widget {
						widget = wibox.container.background,
						appinfo = e.appinfo,
						{
							widget = wibox.container.margin,
							margins = beautiful.launcher_app_margin,
							{
								widget = wibox.widget.textbox,
								text = e.name,
								forced_width = apply_dpi(300),
							}
						}
					}
				)
			end
		end

		self.scrollbar.image = nil
		self.scrollbar.forced_height = nil
		local sample = self.grid:get_children()[1]
		if sample then
			sample = sample.widget.widget
			local _, h = sample:get_preferred_size(awful.screen.focused())
			h = h + beautiful.launcher_app_margin * 2
			if #self.entries_filtered < self.limit then
				h = h * #self.entries_filtered
			else
				h = h * self.limit
			end
			self.scrollbar.forced_height = apply_dpi(h)
			local c = #self.entries_filtered / h
			self.scrollbar.image = cairo.CreateImage(function(cr)
					cr:set_source(gears.color(beautiful.fg_normal))
					cr:rectangle(0, self.scroll / c, 2, self.limit / c)
					cr:fill(((#self.entries_filtered - self.limit) / c))
			end, {2, h})
		end

		self:style()
	end

	function popup_launcher:next()
		if self.chosen < #self.entries_filtered then
			self.chosen = self.chosen + 1
		end

		if self.chosen > self.scroll + self.limit then
			self.scroll = self.scroll + 1
		end
	end

	function popup_launcher:prev()
		if self.chosen ~= 1 then
			self.chosen = self.chosen - 1
		end

		if self.chosen < self.scroll + 1 then
			self.scroll = self.scroll - 1
		end
	end

	function popup_launcher:press(str)
		local idx = self.chosen - self.scroll
		local entry = self.widget:get_children_by_id("grid")[1]:get_children()[idx]
		if entry then
			entry.appinfo:launch()
			notify {
				title = "Launching Application",
				text = entry.widget.widget.text
			}
		else
			awful.spawn.with_shell(str)
			notify {
				title = "Running Command",
				text = str
			}
		end
	end

	function popup_launcher:toggle()
		self.visible = true
		self.chosen = 1
		self.scroll = 0
		self:filter("")

		awful.prompt.run {
			textbox = self.widget:get_children_by_id("prompt")[1],
			bg_cursor = beautiful.fg_normal .. "80",
			changed_callback = function(str) self:filter(str) end,
			keypressed_callback = function(mod, key)
				-- Controls
				local keys = {
					{ n = function() self:next() end, mod = "Control" },
					{ p = function() self:prev() end, mod = "Control" },
					{ Down = function() self:next() end },
					{ Up = function() self:prev() end },
				}
				for _, k in ipairs(keys) do
					if k[key] and (k.mod or true or mod[k.mod]) then
						k[key]()
					end
				end
				self:style()
			end,
			exe_callback = function(cmd)
				self:press(cmd)
			end,
			done_callback = function()
				self.visible = false
			end
		}
	end

	popup_launcher.grid:buttons(gears.table.join(
			awful.button({}, 1, function()
					root.execute_keybinding(nil, "Return")
			end),
			awful.button({}, 4, function()
					popup_launcher:prev()
					popup_launcher:filter(popup_launcher.prompt.text:sub(1, -2))
					popup_launcher:style()
			end),
			awful.button({}, 5, function()
					popup_launcher:next()
					popup_launcher:filter(popup_launcher.prompt.text:sub(1, -2))
					popup_launcher:style()
			end)
	))

	local widget = {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		bg = "#00000030",
		buttons = gears.table.join(
			awful.button({}, 1, function()
					if popup_launcher.visible then
						root.execute_keybinding(nil, "Escape")
					else
						popup_launcher:toggle()
					end
			end),
			awful.button({}, 3, function()
					if popup_launcher.visible then
						root.execute_keybinding(nil, "Escape")
					else
						popup_power:toggle()
					end
			end)
		),
		{
			widget = wibox.widget.imagebox,
			image = cairo.CreateImage(function(cr)
					cr:set_source(gears.color(beautiful.wibar_icon_color))
					shapes.transform(shapes.rounded_rect)
						:translate(4, 4)(cr, 5, 5, 1)
					shapes.transform(shapes.rounded_rect)
						:translate(4, 11)(cr, 5, 5, 1)
					shapes.transform(shapes.rounded_rect)
						:translate(11, 4)(cr, 5, 5, 1)
					shapes.transform(shapes.rounded_rect)
						:translate(11, 11)(cr, 5, 5, 1)
					cr:fill()
			end)
		}
	}

	awful.widget.dashboard = setmetatable(
		{
			power = function() popup_power:toggle() end,
			launcher = function() popup_launcher:toggle() end
		},
		{ __call = function() return widget end }
	)
end

-- Run Commands On Startup --
awful.spawn.run_if_installed {
	"setxkbmap -option keypad:pointerkeys -option compose:paus",
	"xrandr --output DP-1 --mode 1280x1024 --scale 1.3x1.3",
	"xset r rate 250 50 s off -dpms" -- Set keyboard rate and disable dpms
}


-- Keybindings --
local globalkeys = gears.table.join(
	awful.key( -- Focus previous window
		{ "Mod4" }, "j", function() awful.client.focus.byidx(-1) end,
		{ group = "Client", description = "Focus previous client " }
	),
	awful.key( -- Focus next window
		{ "Mod4" }, "k", function() awful.client.focus.byidx(1) end,
		{ group = "Client", description = "Focus next client" }
	),
	awful.key( -- Swap with next window
		{ "Mod4", "Shift" }, "j", function() awful.client.swap.byidx(-1) end,
		{
			group = "Client",
			description = "Swap the current client with the next client"
		}
	),
	awful.key( -- Swap with next window
		{ "Mod4", "Shift" }, "k", function() awful.client.swap.byidx(1) end,
		{
			group = "Client",
			description = "Swap the current client with the previos client"
		}
	),
	-- Screens
	awful.key( -- Focus previous screen
		{ "Mod4", "Control" }, "j", function()
			awful.screen.focus_relative(-1)
		end,
		{
			group = "Screen",
			description = "Focus the previous screen"
		}
	),
	awful.key( -- Focus next screen
		{ "Mod4", "Control" }, "k", function()
			awful.screen.focus_relative(1)
		end,
		{
			group = "Screen",
			description = "Focus the next screen"
		}
	),
	-- Layout
	awful.key(
		{ "Mod4" }, "t", function()
			utils.layout_set(awful.layout.suit.tile.right)
		end,
		{
			group = "Layout",
			description = "Switch to the tiling layout"
		}
	),
	awful.key(
		{ "Mod4" }, "m", function()
			utils.layout_set(awful.layout.suit.max)
		end,
		{
			group = "Layout",
			description = "Switch to the monocle/max layout"
		}
	),
	awful.key(
		{ "Mod4" }, "f", function()
			utils.layout_set(awful.layout.suit.floating)
		end,
		{
			group = "Layout",
			description = "Switch to the floating layout"
		}
	),
	-- Tags
	awful.key(
		{ "Mod4" }, "Tab", function() awful.tag.viewidx(1) end,
		{ group = "Tag", description = "Switch to the next tag" }
	),
	awful.key(
		{ "Mod4", "Shift" }, "Tab", function() awful.tag.viewidx(-1) end,
		{ group = "Tag", description = "Switch to the previous tag" }
	),
	-- Increase master width
	awful.key(
		{ "Mod4" }, "l", function() utils.layout_incmwfact(0.05) end,
		{ group = "Layout", description = "Increase the master width factor" }
	),
	-- Decrease master width
	awful.key(
		{ "Mod4" }, "h", function() utils.layout_incmwfact(-0.05) end,
		{ group = "Layout", description = "Decrease the master width factor" }
	),
	-- Awesome Functions
	awful.key(
		{ "Mod4", "Control" }, "r", awesome.restart,
		{ group = "Awesome", description = "Restart Awesome" }
	),
	awful.key(
		{ "Mod4", "Shift" }, "q", awesome.quit,
		{ group = "Awesome", description = "Quit Awesome" }
	),
	-- Applications --
	awful.key(
		{ "Mod4" }, "Return",
		function()
			utils.launch("Emacs", "emacs --internal-border=20")
		end,
		{ group = "Application", description = "Launch Emacs"}
	),
	awful.key(
		{ "Mod4" }, "b",
		function()
			local AppInfo = lgi.Gio.AppInfo
			local app = AppInfo.get_default_for_type("text/html")
			if app then
				utils.launch(AppInfo.get_name(app), AppInfo.get_executable(app))
			else
				notify {
					title = "Error",
					text = "No default browser found"
				}
			end
		end,
		{ group = "Application", description = "Launch a Browser" }
	),
	-- Volume
	awful.key(
		{ "Mod4" }, "]",
		function()
			awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%", false)
		end,
		{ group = "Volume", description = "Increase Volume"}
	),
	awful.key(
		{ "Mod4" }, "[",
		function()
			awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%", false)
		end,
		{ group = "Volume", description = "Decrease Volume"}
	),
	awful.key(
		{ "Mod4" }, "\\",
		function()
			awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
		end,
		{ group = "Volume", description = "Decrease Volume"}
	),
	-- Screenshot
	awful.key(
		{ "Mod4" }, "s", awful.widget.screenshot.popup,
		{ group = "Screenshot", description = "Take A Screenshot" }
	),
	-- Dashboard
	awful.key(
		{ "Mod4" }, "q", awful.widget.dashboard.power,
		{ group = "Dashboard", description = "Open The Power Menu" }
	),
	-- Launcher
	awful.key(
		{ "Mod4" }, "p", awful.widget.dashboard.launcher,
		{ group = "Launcher", description = "Open The Application Launcher" }
	)
)

do -- Move and resize clients using the arrow keys
	local t = {
		["Up"] = { 0, 10, 0, 10},
		["Down"] = { 0, -10, 0, -10},
		["Left"] = { -10, 0, -10, 0},
		["Right"] = { 10, 0, 10, 0},
	}

	for direction, coords in pairs(t) do
		clientkeys = gears.table.join(
			clientkeys,
			awful.key(
				{ "Mod4" }, direction, function(c)
					c:relative_move(coords[1], coords[2], 0, 0)
				end,
				{
					group = "Client",
					description = "Move the client " .. direction:lower()
				}
			),
			awful.key(
				{ "Mod4" }, direction, function(c)
					c:relative_move(0, 0, coords[3], coords[4])
				end,
				{
					group = "Client",
					description = "Resize the client " .. direction:lower()
				}
			)
		)
	end
end

-- Bind Keybindings to Tags
for i = 1, beautiful.tag_amount or 5 do
	globalkeys = gears.table.join(
		globalkeys,
		-- View tag only.
		awful.key(
			{ "Mod4" }, i,
			function()
				local tag = awful.screen.focused().tags[i]
				if tag then tag:view_only() end
			end,
			{ group = "Tag", description = "Switch to tag " .. i }
		),
		-- Toggle tag display.
		awful.key(
			{ "Mod4", "Control" }, i,
			function()
				local tag = awful.screen.focused().tags[i]
				if tag then awful.tag.viewtoggle(tag) end
			end,
			{ group = "Tag", description = "Toggle view tag " .. i }
		),
		-- Move client to tag.
		awful.key(
			{ "Mod4", "Shift" }, i,
			function()
				if not client.focus then return end
				local tag = client.focus.screen.tags[i]
				if tag then client.focus:move_to_tag(tag) end
			end,
			{ group = "Tag", description = "Move client to tag " .. i }
		)
	)
end
root.keys(globalkeys)


-- Clients
local clientkeys = gears.table.join(
	awful.key(
		{ "Mod4", "Shift" }, "c", function(c) c:kill() end,
		{ group = "Client", description = "Kill the client" }
	),
	awful.key(
		{ "Mod4", "Shift" }, "f", function(c) c.floating = not c.floating end,
		{ group = "Client", description = "Toggle floating" }
	),
	awful.key(
		{ "Mod4", "Shift" }, "o", function(c) c:move_to_screen() end,
		{ group = "Client", description = "Move client to the current screen" }
	),
	awful.key(
		{ "Mod4" }, "o", function(c) c:move_to_screen() end,
		{ group = "Client", description = "Move client to the current screen" }
	)
)

local clientbuttons = gears.table.join(
	awful.button({}, 1, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
	end),
	awful.button({ "Mod4" }, 1, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.move(c)
	end),
	awful.button({ "Mod4" }, 3, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.resize(c)
	end)
)



-- Rules --
awful.rules.rules = {
	{
		rule = {},
		properties = {
			border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			size_hints_honor = false,
			focus = awful.client.focus.filter,
			raise = true,
			keys = clientkeys,
			buttons = clientbuttons,
			screen = awful.screen.preferred,
			placement = awful.placement.no_overlap+awful.placement.no_offscreen,
		}
	}
}



-- Wibar --
awful.screen.connect_for_each_screen(function(s)
		local tags = {}
		for i = 1, beautiful.tag_amount or 5 do
			tags[i] = tostring(i)
		end
		awful.tag(tags, s, awful.layout.suit.tile.right)
		s.wibar = awful.wibar {
			screen = s,
			ontop = false,
			position = beautiful.wibar_position or "top"
		}
		s.wibar:setup {
			layout = wibox.layout.align.vertical,
			spacing = 0,
			{ widget = wibox.container.background },
			{
				widget = wibox.container.margin,
				margins = apply_dpi(7.5),
				{
					layout = wibox.layout.align.horizontal,
					expand = "none",
					{
						layout = wibox.layout.fixed.horizontal,
						spacing = apply_dpi(10),
						awful.widget.dashboard(),
						awful.widget.taglist_styled(s),
						awful.widget.layout()
					},
					{
						layout = wibox.layout.fixed.horizontal,
						awful.widget.tasklist_styled(s)
					},
					{
						layout = wibox.layout.fixed.horizontal,
						spacing = apply_dpi(10),
						awful.widget.screenshot(),
						awful.widget.network(),
						awful.widget.volume(),
						awful.widget.date()
					},
				}
			},
			{
				widget = wibox.container.background,
				bg = beautiful.border_focus,
				{
					widget = wibox.container.constraint,
					forced_height = beautiful.border_width
				}
			}
		}
end)


-- Sets the wallpaper
gears.wallpaper.set(beautiful.wallpaper or "#222222")
screen.connect_signal("property::geometry", function()
		gears.wallpaper.set(beautiful.wallpaper or "#222222")
end)


client.connect_signal("manage", function(c)
		if c.first_tag.layout == awful.layout.suit.max then
			c.border_width = 0
		end
		if not awesome.startup then return end
		if c.size_hints.user_position then return end
		if c.size_hints.program_position then return end
		awful.placement.no_offscreen(c)
end)


client.connect_signal("focus", function(c)
		c.border_color = beautiful.border_focus
end)


client.connect_signal("unfocus", function(c)
		c.border_color = beautiful.border_normal
end)


client.connect_signal("property::floating", function(c)
		if c.floating then
			awful.titlebar.show(c)
			c.border_width = 0
		else
			awful.titlebar.hide(c)
			if root.tags()[1].layout ~= awful.layout.suit.max then
				c.border_width = beautiful.border_width
			end
		end
end)


client.connect_signal("request::titlebars", function(c)
		local function btn_create(color, func)
			local btn = wibox.widget {
				buttons = awful.button({}, 1, func),
				widget = wibox.container.background,
				shape = gears.shape.circle,
				bg = color,
				{
					widget = wibox.container.constraint,
					forced_height = apply_dpi(15),
					forced_width = apply_dpi(15)
				}
			}

			btn:connect_signal("mouse::enter", function()
					btn.bg = cairo.get_rgb_as_hex(btn.bg) .. "90"
			end)

			btn:connect_signal("mouse::leave", function()
					btn.bg = cairo.get_rgb_as_hex(btn.bg)
			end)

			return btn
		end

		awful.titlebar(c, {size = apply_dpi(40)}):setup {
			layout = wibox.layout.align.horizontal,
			wibox.widget {},
			{
				layout = wibox.layout.flex.horizontal,
				buttons = gears.table.join(
					awful.button({}, 1, function()
							awful.mouse.client.move(c)
					end),
					awful.button({}, 3, function()
							awful.mouse.client.resize(c)
					end)
				)
			},
			{
				widget = wibox.container.background,
				layout = wibox.layout.fixed.horizontal,
				spacing = apply_dpi(10),
				btn_create(beautiful.titlebar_btn_max, function()
						c.maximized = not c.maximized
				end),
				btn_create(beautiful.titlebar_btn_min, function()
						c.minimized = not c.minimized
				end),
				btn_create(beautiful.titlebar_btn_close, function()
						c:kill()
				end),
				wibox.widget {}
			}
		}


end)


gears.timer { -- More frequent garbage collection
	timeout = 30,
	autostart = true,
	callback = collectgarbage
}
