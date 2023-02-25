--[[
	Smeueg's AwesomeWM Configuration File

	REQUIRES:
	- pactl (Pulseaudio)
	- nmcli (NetworkManager)

	TODO: Turn widget functions into a wibox.widget (primarily for the volume
	widget)
	TODO: Add a dashboard (as a launcher and powermenu) (WIP):
	- PowerMenu: ✓
	- Launcher: ✗
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
local pi = math.pi


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
	-- Dashboard
	profile_picture = os.getenv("HOME") .. "/.config/awesome/pfp.png",
	-- Calendar
	calendar_fg_normal = "#43413F",
	-- Notifications
	notification_border_color = "#FE8019",
	-- Taglist
	taglist_fg_focus = "#FABD2F",
	taglist_fg_normal = "#504945",
	-- Tasklist
	tasklist_bg_focus = "#00000030"
}

naughty.config.spacing = apply_dpi(5)

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


-- Custom Functions --
function string:upper_first()
	return self:sub(1, 1):upper() .. self:sub(2)
end
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

function cairo.CreateImage(body)
	local tmp = {}
	tmp.image = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
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
		{ "Mod4" }, "s", function() awful.widget.screenshot_popup() end,
		{ group = "Screenshot", description = "Take A Screenshot" }
	),
	-- Dashboard
	awful.key(
		{ "Mod4" }, "q", function() awful.widget.dashboard_popup() end,
		{ group = "Dashboard", description = "Open The Dashboard" }
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
for i = 1, 5 do
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


local widgets = {}
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

function awful.widget.volume()
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


	return widget
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
						id = "icon_role"
					}
				}
			}
		}
end

function awful.widget.network()
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

	return widget
end

function awful.widget.screenshot_popup()
	if not gears.filesystem.find_executable("import") then
		notify{
			title = "Cannot Take Screenshot",
			text = "`import` (from imagemagick) isn't installed"
		}
		return
	end

	if root.screenshot_popup then
		root.screenshot_popup.keygrabber:stop()
		root.screenshot_popup.visible = false
		root.screenshot_popup = nil
		return
	end

	local controls = {}
	local function template_option(text, callback)
		return {
			widget = wibox.container.background,
			buttons = awful.button({}, 1, function() controls.press() end),
			shape = gears.shape.rounded_rect_auto,
			callback = callback,
			id = "option",
			{
				widget = wibox.container.margin,
				margins = apply_dpi(10),
				{
					widget = wibox.widget.textbox,
					markup = string.format("<big><b>%s</b></big>", text),
					align = "center",
					valign = "center"
				}
			}
		}
	end

	local function template_path()
		return os.date("/tmp/Screenshot %d-%m-%Y %X.png")
	end

	root.screenshot_popup = awful.popup {
		placement = awful.placement.centered,
		border_width = beautiful.border_width,
		border_color = beautiful.border_focus,
		ontop = true,
		widget = {
			widget = wibox.container.margin,
			margins = apply_dpi(10),
			{
				layout = wibox.layout.flex.horizontal,
				spacing = apply_dpi(2.5),
				forced_height = apply_dpi(100),
				forced_width = apply_dpi(200),
				template_option("Full", function()
						local path = template_path()
						awful.spawn.easy_async_with_shell(
							string.format(
								"sleep 0.05; import -window root '%s'",
								path
							),
							function(_, _, _, exit_code)
								if exit_code ~= 0 then return end
								notify {
									title = "Took Screenshot",
									text = "As " .. path
								}
							end
						)
				end),
				template_option("Partial", function()
						local path = template_path()
						awful.spawn.easy_async_with_shell(
							string.format("sleep 0.05; import '%s'", path),
							function(_, _, _, exit_code)
								if exit_code ~= 0 then return end
								notify {
									title = "Took Screenshot",
									text = "As " .. path
								}
							end
						)
				end),
			}
		}
	}

	root.screenshot_popup.chosen = 1
	local opts = root.screenshot_popup.widget:get_children_by_id("option")

	local function update()
		for i, w in ipairs(opts) do
			if i == root.screenshot_popup.chosen then
				w.bg = "#00000030"
				w.fg = beautiful.border_focus
			else
				w.bg = nil
				w.fg = beautiful.fg_normal
			end
		end
	end

	function controls.press()
		opts[root.screenshot_popup.chosen].callback()
		awful.widget.screenshot_popup()
	end

	function controls.select_next()
		if not (root.screenshot_popup.chosen < #opts) then return end
		root.screenshot_popup.chosen = root.screenshot_popup.chosen + 1
		update()
	end

	function controls.select_prev()
		if not (root.screenshot_popup.chosen > 1) then return end
		root.screenshot_popup.chosen = root.screenshot_popup.chosen - 1
		update()
	end

	-- Mouse Controls
	for i, w in ipairs(opts) do
		w:connect_signal("mouse::enter", function()
				root.screenshot_popup.chosen = i
				update()
		end)
	end

	-- Keyboard Controls
	root.screenshot_popup.keygrabber = awful.keygrabber {
		autostart = true,
		keybindings = {
			{{}, "h", controls.select_prev},
			{{}, "l", controls.select_next},
			{{"Control"}, "p", controls.select_prev},
			{{"Control"}, "n", controls.select_next},
			{{"Control"}, "g", awful.widget.screenshot_popup},
			{{}, "Escape", awful.widget.screenshot_popup},
			{{}, "Return", controls.press}
		}
	}

	update()
end

function awful.widget.screenshot()
	return {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		bg = "#00000030",
		buttons = awful.button({}, 1, awful.widget.screenshot_popup),
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
end

function awful.widget.layout()
	if type(awful.widget.layout) ~= "function" then
		return awful.widget.layout
	end

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

	awful.widget.layout = wibox.widget {
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
			awful.widget.layout:get_children()[1].image = icons.tile
		elseif t.layout == awful.layout.suit.max then
			awful.widget.layout:get_children()[1].image = icons.max
		else
			awful.widget.layout:get_children()[1].image = icons.float
		end
	end

	update(root.tags()[1])
	tag.connect_signal("property::layout", update)

	return awful.widget.layout
end

function awful.widget.dashboard_popup()
	if root.dashboard_popup then
		root.dashboard_popup.keygrabber:stop()
		root.dashboard_popup.visible = false
		root.dashboard_popup = nil
		return
	end

	local function template_option(image, callback)
		return {
			widget = wibox.container.background,
			buttons = awful.button({}, 1, function() press.f() end),
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
						image = image,
						forced_height = apply_dpi(40),
						forced_width = apply_dpi(40)
					}
				}
			}
		}
	end

	local function separator(orientation)
		return wibox.widget {
			widget = wibox.widget.separator,
			orientation = orientation,
			color = beautiful.fg_normal .. "10",
			forced_width = orientation == "horizontal" and 1 or apply_dpi(30),
			forced_height = orientation == "vertical" and 1 or apply_dpi(30)
		}
	end

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
		end)
	}

	root.dashboard_popup = awful.popup {
		placement = function(p)
			awful.placement.top_left(p, {
					offset = { y = beautiful.wibar_height }
			})
		end,
		ontop = true,
		widget = {
			widget = wibox.container.margin,
			margins = apply_dpi(10),
			{
				widget = wibox.container.background,
				layout = wibox.layout.grid,
				homogeneous = false,
				forced_num_cols = 3,
				spacing = apply_dpi(10)
			}
		}
	}

	local function add_to_popup(...)
		root.dashboard_popup.widget.widget:add_widget_at(...)
	end

	-- PFP, User, and Hostname
	add_to_popup(wibox.widget {
			widget = wibox.container.background,
			shape = gears.shape.circle,
			shape_border_width = apply_dpi(3),
			shape_border_color = beautiful.border_focus,
			shape_clip = true,
			{
				widget = wibox.container.place,
				valign = "center",
				halign = "center",
				forced_height = apply_dpi(80),
				forced_width =  apply_dpi(80),
				{
					widget = wibox.widget.imagebox,
					image = beautiful.profile_picture,
					forced_height = apply_dpi(64),
					forced_width = apply_dpi(64),
					resize = true,
				}
			}

	}, 1, 1, 2, 1)
	add_to_popup(wibox.widget{
			widget = wibox.widget.textbox,
			font = utils.font_size(12),
			markup = string.format(
				"Welcome to %s, %s",
				utils.style(os.getenv("HOSTNAME"):upper_first(), {
						foreground = beautiful.border_focus,
						font_weight = "bold"
				}),
				utils.style(os.getenv("USER"):upper_first(), {
						foreground = beautiful.border_focus,
						font_weight = "bold"
				})
			)
	}, 1, 2, 1, 2)
	add_to_popup(wibox.widget{
			widget = wibox.widget.textbox,
			font = utils.font_size(12),
			markup = "Hope you enjoy your stay!"
	}, 2, 2, 1, 2)
	add_to_popup(separator("horizontal"), 3, 1, 1, 3)
	-- Calendar
	add_to_popup(wibox.widget {
			widget = wibox.container.background,
			shape = gears.shape.rounded_rect_auto,
			shape_border_width = beautiful.border_width,
			shape_border_color = beautiful.fg_normal .. "10",
			{
				widget = wibox.container.margin,
				margins = apply_dpi(10),
				{
					widget = wibox.widget.calendar.month,
					font = beautiful.font:match("(.*) [0-9]+") .. " 13",
					long_weekdays = true,
					spacing = apply_dpi(10),
					date = os.date("*t"),
					fn_embed = function(widget, flag, date)
						local styles = {
							month = {
								padding = apply_dpi(5),
								fg = beautiful.border_focus,
								bg = "#00000030"
							},
							normal = {fg = beautiful.calendar_fg_normal},
							header = {fg = beautiful.fg_normal},
							weekday = {},
							focus = {}
						}
						if widget.text then
							widget.align = "center"
							widget.valign = "center"
							if flag == "focus" or flag == "month" then
								widget.markup = string.format(
									"<b>%s</b>",
									widget.text
								)
							end
						end

						return wibox.widget {
							widget = wibox.container.background,
							shape = gears.shape.rounded_rect_auto,
							bg = styles[flag].bg,
							fg = styles[flag].fg,
							widget,
						}
					end
				}
			}

	}, 4, 1, 1, 3)
	add_to_popup(separator("horizontal"), 5, 1, 1, 3)

	-- Power Menu
	local power_menu = wibox.widget {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		shape_border_width = beautiful.border_width,
		shape_border_color = beautiful.fg_normal .. "10",
		chosen = 1,
		{
			widget = wibox.container.margin,
			margins = apply_dpi(10),
			{
				layout = wibox.layout.grid,
				spacing = apply_dpi(2),
				orientation = "horizontal",
				expand = true,
				template_option(icons.shutdown, function()
						notify {text = "Shutting Down System"}
						awful.spawn("systemctl poweroff")
				end),
				template_option(icons.reboot, function()
						notify {text = "Rebooting System"}
						awful.spawn("systemctl reboot")
				end),
				template_option(icons.suspend, function()
						notify {text = "Suspending System"}
						awful.spawn("systemctl suspend")
				end)
			}
		}
	}

	local opts = power_menu:get_children_by_id("opt")
	local controls = {}

	local function update()
		for i, w in ipairs(opts) do
			if i == power_menu.chosen then
				w.bg = "#00000030"
				w.fg = beautiful.border_focus
			else
				w.bg = nil
				w.fg = beautiful.fg_normal
			end
		end
	end

	function controls.press()
		opts[power_menu.chosen].callback()
	end

	function controls.next()
		if not (power_menu.chosen < #opts) then return end
		power_menu.chosen = power_menu.chosen + 1
		update()
	end

	function controls.prev()
		if not (power_menu.chosen > 1) then return end
		power_menu.chosen = power_menu.chosen - 1
		update()
	end

	for i, w in ipairs(opts) do
		w:connect_signal("mouse::enter", function()
				power_menu.chosen = i
				update()
		end)
	end

	root.dashboard_popup.keygrabber = awful.keygrabber {
		autostart = true,
		keybindings = {
			{{}, "h", controls.prev},
			{{}, "l", controls.next},
			{{"Control"}, "p", controls.prev},
			{{"Control"}, "n", controls.next},
			{{"Control"}, "g", awful.widget.dashboard_popup},
			{{}, "Escape", awful.widget.dashboard_popup},
			{{}, "Return", controls.press}
		}
	}

	update()
	root.dashboard_popup.widget.widget:add_widget_at(power_menu, 6, 1, 1, 3)
end

function awful.widget.dashboard()
	if not (type(awful.widget.dashboard) == "function") then
		return awful.widget.dashboard
	end

	local icon = cairo.CreateImage(function(cr)
			cr:set_source(gears.color(beautiful.wibar_icon_color))
			shapes.transform(shapes.rounded_rect):translate(4, 4)(cr, 5, 5, 1)
			shapes.transform(shapes.rounded_rect):translate(4, 11)(cr, 5, 5, 1)
			shapes.transform(shapes.rounded_rect):translate(11, 4)(cr, 5, 5, 1)
			shapes.transform(shapes.rounded_rect):translate(11, 11)(cr, 5, 5, 1)
			cr:fill()
	end)

	awful.widget.dashboard = {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		bg = "#00000030",
		buttons = awful.button({}, 1, awful.widget.dashboard_popup),
		{
			widget = wibox.widget.imagebox,
			image = icon
		}
	}

	return awful.widget.dashboard
end


-- Wibar --
awful.screen.connect_for_each_screen(
	function(s)
			awful.tag({ "1", "2", "3", "4", "5" }, s, awful.layout.suit.tile.right)
			s.wibar = awful.wibar { screen = s }
			s.wibar:setup {
				widget = wibox.container.margin,
				margins = 7.5,
				{
					layout = wibox.layout.align.horizontal,
					expand = "none",
					{ -- Left Widgets
						layout = wibox.layout.fixed.horizontal,
						spacing = 10,
						awful.widget.dashboard(),
						awful.widget.taglist_styled(s),
						awful.widget.layout()
					},
					{ -- Center Widgets
						layout = wibox.layout.fixed.horizontal,
						spacing = 10,
						awful.widget.tasklist_styled(s)
					},
					{ -- Right Widgets
						layout = wibox.layout.fixed.horizontal,
						spacing = 10,
						widgets.screenshot,
						awful.widget.screenshot(),
						awful.widget.network(),
						awful.widget.volume(),
						awful.widget.date()
					}
				}
		}
	end
)


-- Sets the wallpaper
gears.wallpaper.set(beautiful.wallpaper or "#222222")
screen.connect_signal("property::geometry",
	function()
		gears.wallpaper.set(beautiful.wallpaper or "#222222")
	end
)


client.connect_signal(
	"manage",
	function(c)
		if c.first_tag.layout == awful.layout.suit.max then
			c.border_width = 0
		end
		if not awesome.startup then return end
		if c.size_hints.user_position then return end
		if c.size_hints.program_position then return end
		awful.placement.no_offscreen(c)
	end
)


client.connect_signal("focus", function(c)
		c.border_color = beautiful.border_focus
end)


client.connect_signal("unfocus", function(c)
		c.border_color = beautiful.border_normal
end)

client.connect_signal(
	"property::floating",
	function(c)
		if c.floating then
			awful.titlebar.show(c)
			c.border_width = 0
		else
			awful.titlebar.hide(c)
			if root.tags()[1].layout ~= awful.layout.suit.max then
				c.border_width = beautiful.border_width
			end
		end
	end
)


client.connect_signal(
	"request::titlebars",
	function(c)
		local function btn_create(color, func)
			return {
				buttons = awful.button({}, 1, func),
				widget = wibox.container.background,
				shape = gears.shape.circle,
				shape_border_width = apply_dpi(4),
				shape_border_color = color .. "90",
				bg = color,
				{
					widget = wibox.container.constraint,
					forced_height = apply_dpi(17.5),
					forced_width = apply_dpi(17.5)
				}
			}
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
				btn_create(beautiful.titlebar_btn_close,
					function() c:kill()
				end),
				wibox.widget {}
			}
		}
	end
)


gears.timer { -- More frequent garbage collection
	timeout = 30,
	autostart = true,
	callback = collectgarbage
}
