--[[
	Smeueg's AwesomeWM Configuration File
	Documenation for AwesomeWM:
	https://awesomewm.org/doc/api/documentation/01-readme.md.html

	REQUIREMENTS:
	- pactl (Pulseaudio)
	- nmcli (NetworkManager)

	TODO: Provide a lock screen
	TODO: Show a popup to display keybindings
	TODO: Add USEFULL comments
	TODO: Better code structure
	TODO: More efficient resource usage
	TODO: Higher quality icons (use {40, 40} instead of {20, 20})
	TODO: (WIP) Create a right click menu to close the clients on the wibar using awful.menu
--]]

-- Import Libraries
pcall(require, "luarocks.loader") -- Make sure LuaRocks packages is loaded
require("awful.autofocus")
local beautiful = require("beautiful")
local naughty = require("naughty")
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local lgi = require("lgi")
local dpi = beautiful.xresources.apply_dpi
local notify = naughty.notify
local cairo = lgi.cairo
local Gio = lgi.Gio

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
do
	local settings = {}
	local colors = {
		bg = "#282828",
		red = "#D65D0E",
		green = "#B8BB26",
		yellow = "#FABD2F",
		blue = "#458588",
		magenta = "#B16286",
		cyan = "#689D6A",
		white = "#EBDBB2",
		black = "#3C3836",
		black_bright = "#32302f",
		black_brighter = "#504945",
		orange = "#FE8019"
	}
	-- Default
	settings.wallpaper = colors.bg
	settings.font = "JetBrainsMono Nerd Font Mono 11"
	settings.bg_normal = colors.black_bright
	settings.fg_normal = colors.white
	settings.icon_color = colors.yellow
	settings.shape_universal = function (cr, width, height)
		gears.shape.rounded_rect(cr, width, height, dpi(5))
	end
	-- Windows
	settings.useless_gap = dpi(10)
	settings.border_width = dpi(2)
	settings.border_focus = colors.orange
	settings.border_normal = settings.wallpaper
	-- Titlebar
	settings.titlebar_bg = settings.bg_normal
	settings.titlebar_btn_max = colors.green
	settings.titlebar_btn_min = colors.yellow
	settings.titlebar_btn_close = colors.red
	-- Wibar
	settings.wibar_height = dpi(50)
	settings.wibar_padding = dpi(7.5)
	settings.wibar_position = "top"
	-- Notifications
	settings.notification_border_color = settings.border_focus
	settings.notification_shape = nil
	naughty.config.spacing = dpi(5)
	-- Tags
	settings.tag_amount = 3
	settings.taglist_fg_focus = colors.yellow
	settings.taglist_fg_normal = colors.black_brighter
	-- Tasklist
	settings.tasklist_bg_focus = "#00000030"
	settings.tasklist_inner_margin = dpi(5)
	-- Dashboard
	settings.dashboard_margins = dpi(10)
	settings.dashboard_text_width = dpi(300)
	settings.dashboard_limit = 10
	-- Edge Snapping
	settings.snap_shape = gears.shape.rectangle
	settings.snap_border_width = settings.border_width
	settings.snap_bg = settings.border_focus

	beautiful.init(settings)
end

local widgets = require("ui.widgets")
local modkey = "Mod4"


-- Custom Functions --
function string:upper_first()
	return self:sub(1, 1):upper() .. self:sub(2)
end

function awful.spawn.launch(name, cmd)
	notify { title = "Launching Application", text = name }
	awful.spawn(cmd, {
			tag = awful.screen.focused().selected_tag
	})
end

function awful.layout.set_all(layout)
	-- Sets a layout for every tag
	for _, t in ipairs(root.tags()) do
		awful.layout.set(layout, t)
	end
end

function awful.layout.incmwf_all(factor)
	if awful.layout.get() ~= awful.layout.suit.tile.right then return end
	for _, t in ipairs(root.tags()) do awful.tag.incmwfact(factor, t) end
end

function awful.layout.incnmaster(n)
	if awful.layout.get() ~= awful.layout.suit.tile.right then return end
	if root.tags()[1].master_count == 1 and n < 0 then return end
	for _, t in ipairs(root.tags()) do awful.tag.incnmaster(n, t, true) end
	notify {
		title = "Current Master Count",
		text = tostring(root.tags()[1].master_count),
		position = "top_left",
		timeout = 2
	}
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

function cairo.get_rgb_as_hex(pattern)
	local _, r, g, b = pattern:get_rgba()
	r = math.floor(r * 255 + 0.5)
	g = math.floor(g * 255 + 0.5)
	b = math.floor(b * 255 + 0.5)
	return string.format("#%02X%02X%02X", r, g, b)
end

function root.execute_keybinding(modifiers, key)
	local conversion = {
		Mod4 = "Super_L",
		Control = "Control_L",
		Shift = "Shift_L",
		Mod1 = "Alt_L"
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

function gears.filesystem.find_executable(executable)
	local path
	for dir in string.gmatch(os.getenv("PATH"), "([^:]+)") do
		path = dir .. "/" .. executable
		if gears.filesystem.file_executable(path) then return path end
	end
	return nil
end

---@param cmds table<string>
---@return number
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
	return 1
end

-- Custom Shapes --
function gears.shape.rounded_rect_auto(cr, width, height)
	gears.shape.rounded_rect(cr, width, height, dpi(5))
end


-- Custom Widgets --
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
			awful.button({}, 4, function() awful.tag.viewidx(-1) end),
			awful.button({}, 5, function() awful.tag.viewidx(1) end)
		),
		layout = {
			layout = wibox.layout.fixed.horizontal,
			spacing = dpi(10)
		},
		widget_template = {
			widget = wibox.container.margin,
			margins = dpi(1),
			{
				id = "icon",
				widget = wibox.container.background,
				shape = gears.shape.circle,
				shape_border_width = dpi(2),
				forced_height = dpi(15),
				forced_width = dpi(15),
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
			awful.button({ }, 3, function(c)
					local menu = awful.menu {
						{"Close", function() c:kill() end, nil}
					}
					menu:show()

					-- Hide and destroy the menu
					menu.wibox.widget:connect_signal("mouse::leave", function()
							menu:hide()
							menu = nil
					end)
			end),
			awful.button({ }, 4, function() awful.client.focus.byidx(1) end),
			awful.button({ }, 5, function() awful.client.focus.byidx(-1) end)
		),
		widget_template = {
			widget = wibox.container.background,
			id = "background_role",
			{
				widget = wibox.container.margin,
				margins = dpi(beautiful.tasklist_inner_margin),
				{
					widget = wibox.container.place,
					halign = "center",
					{
						widget = wibox.widget.imagebox,
						id = "icon_role",
					}
				}
			},
			create_callback = function(self, c)
				local client = c
				while not client.icon_sizes[1] do
					client = client.transient_for
				end
				self:get_children_by_id("icon_role")[1].image = client.icon
			end
		}
	}
end

-- Run Commands On Startup --
awful.spawn.run_if_installed {
	"setxkbmap -option keypad:pointerkeys -option compose:paus",
	"xrandr --output DP1 --mode 1280x1024 --scale 1.3x1.3",
	"xset r rate 250 50 s off -dpms" -- Set keyboard rate and disable dpms
}


-- Keybindings --
local globalkeys = gears.table.join(
	awful.key( -- Focus previous window
		{ modkey }, "k", function() awful.client.focus.byidx(1) end,
		{ group = "Client", description = "Focus previous client " }
	),
	awful.key( -- Focus next window
		{ modkey }, "j", function() awful.client.focus.byidx(-1) end,
		{ group = "Client", description = "Focus next client" }
	),
	awful.key( -- Swap with next window
		{ modkey, "Shift" }, "k", function() awful.client.swap.byidx(1) end,
		{
			group = "Client",
			description = "Swap the current client with the next client"
		}
	),
	awful.key( -- Swap with next window
		{ modkey, "Shift" }, "j", function() awful.client.swap.byidx(-1) end,
		{
			group = "Client",
			description = "Swap the current client with the previos client"
		}
	),
	-- Screens
	awful.key( -- Focus previous screen
		{ modkey, "Control" }, "j", function()
			awful.screen.focus_relative(-1)
		end,
		{
			group = "Screen",
			description = "Focus the previous screen"
		}
	),
	awful.key( -- Focus next screen
		{ modkey, "Control" }, "k", function()
			awful.screen.focus_relative(1)
		end,
		{
			group = "Screen",
			description = "Focus the next screen"
		}
	),
	-- Layout
	awful.key(
		{ modkey }, "t", function()
			awful.layout.set_all(awful.layout.suit.tile.right)
		end,
		{
			group = "Layout",
			description = "Switch to the tiling layout"
		}
	),
	awful.key(
		{ modkey }, "m", function()
			awful.layout.set_all(awful.layout.suit.max)
		end,
		{
			group = "Layout",
			description = "Switch to the monocle/max layout"
		}
	),
	awful.key(
		{ modkey }, "f", function()
			awful.layout.set_all(awful.layout.suit.floating)
		end,
		{
			group = "Layout",
			description = "Switch to the floating layout"
		}
	),
	-- Tags
	awful.key(
		{ modkey }, "Tab", function() awful.tag.viewidx(1) end,
		{ group = "Tag", description = "Switch to the next tag" }
	),
	awful.key(
		{ modkey, "Shift" }, "Tab", function() awful.tag.viewidx(-1) end,
		{ group = "Tag", description = "Switch to the previous tag" }
	),
	-- Master Stack
	awful.key(
		{ modkey }, "l", function() awful.layout.incmwf_all(0.05) end,
		{ group = "Layout", description = "Increase the master width factor" }
	),
	awful.key(
		{ modkey }, "h", function() awful.layout.incmwf_all(-0.05) end,
		{ group = "Layout", description = "Decrease the master width factor" }
	),
	awful.key(
		{ modkey, "Control" }, "k", function() awful.layout.incnmaster(1) end,
		{ group = "Layout", description = "Increase the number of master windows" }
	),
	awful.key(
		{ modkey, "Control" }, "j", function() awful.layout.incnmaster(-1) end,
		{ group = "Layout", description = "Decrease the number of master windows" }
	),
	-- Awesome Functions
	awful.key(
		{ modkey, "Control" }, "r", awesome.restart,
		{ group = "Awesome", description = "Restart Awesome" }
	),
	awful.key(
		{ modkey, "Shift" }, "q", awesome.quit,
		{ group = "Awesome", description = "Quit Awesome" }
	),
	-- Applications --
	awful.key(
		{ modkey }, "Return",
		function()
			awful.spawn.launch("Emacs", "emacs")
		end,
		{ group = "Application", description = "Launch Emacs"}
	),
	awful.key(
		{ modkey }, "b",
		function()
			local AppInfo = Gio.AppInfo
			local app = AppInfo.get_default_for_type("text/html")
			if app then
				awful.spawn.launch(AppInfo.get_name(app), AppInfo.get_executable(app))
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
		{ modkey }, "]",
		function()
			awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%", false)
		end,
		{ group = "Volume", description = "Increase The Volume By 5"}
	),
	awful.key(
		{}, "XF86AudioRaiseVolume",
		function()
			awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%", false)
		end,
		{ group = "Volume", description = "Increase The Volume By 5"}
	),
	awful.key(
		{ modkey }, "[",
		function()
			awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%", false)
		end,
		{ group = "Volume", description = "Decrease The Volume By 5"}
	),
	awful.key(
		{ }, "XF86AudioLowerVolume",
		function()
			awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%", false)
		end,
		{ group = "Volume", description = "Decrease The Volume By 5"}
	),
	awful.key(
		{ modkey, "Control" }, "]",
		function()
			awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +1%", false)
		end,
		{ group = "Volume", description = "Increase The Volume By 1"}
	),
	awful.key(
		{ modkey, "Control" }, "[",
		function()
			awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ -1%", false)
		end,
		{ group = "Volume", description = "Decrease The Volume By 1"}
	),
	awful.key(
		{ }, "XF86AudioMute",
		function()
			awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
		end,
		{ group = "Volume", description = "(Un)Mute Sound"}
	),
	awful.key(
		{ modkey }, "\\",
		function()
			awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
		end,
		{ group = "Volume", description = "(Un)Mute Sound"}
	),
	-- Brightness
	awful.key(
		{ }, "XF86MonBrightnessUp",
		function() awful.spawn.run_if_installed { "brightnessctl s +1%" } end,
		{ group = "Brightness", description = "Increase the screen's brightness"}
	),
	awful.key(
		{ }, "XF86MonBrightnessDown",
		function() awful.spawn.run_if_installed { "brightnessctl s 1%-" } end,
		{ group = "Brightness", description = "Decrease the screen's brightness"}
	),
	-- Screenshot
	awful.key(
		{ modkey }, "s", awful.widget.screenshot.popup,
		{ group = "Screenshot", description = "Take A Screenshot" }
	),
	-- Dashboard
	awful.key(
		{ modkey }, "p", awful.widget.dashboard.popup,
		{ group = "Dashboard", description = "Open The Power Menu" }
	)
)

-- Bind Keybindings to Tags
for i = 1, beautiful.tag_amount or 5 do
	globalkeys = gears.table.join(
		globalkeys,
		-- View tag only.
		awful.key(
			{ modkey }, i,
			function()
				local tag = awful.screen.focused().tags[i]
				if tag then tag:view_only() end
			end,
			{ group = "Tag", description = "Switch to tag " .. i }
		),
		-- Toggle tag display.
		awful.key(
			{ modkey, "Control" }, i,
			function()
				local tag = awful.screen.focused().tags[i]
				if tag then awful.tag.viewtoggle(tag) end
			end,
			{ group = "Tag", description = "Toggle view tag " .. i }
		),
		-- Move client to tag.
		awful.key(
			{ modkey, "Shift" }, i,
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
		{ modkey }, "q", function(c) c:kill() end,
		{ group = "Client", description = "Kill the client" }
	),
	awful.key(
		{ modkey, "Shift" }, "f", function(c) c.floating = not c.floating end,
		{ group = "Client", description = "Toggle floating" }
	),
	awful.key(
		{ modkey, "Shift" }, "o", function(c) c:move_to_screen() end,
		{ group = "Client", description = "Move client to the current screen" }
	),
	awful.key(
		{ modkey }, "o", function(c) c:move_to_screen() end,
		{ group = "Client", description = "Move client to the current screen" }
	)
)

do -- Move and resize clients using the arrow keys
	local t = {
		["Down"] = { 0, 10 },
		["Up"] = { 0, -10 },
		["Left"] = { -10, 0 },
		["Right"] = { 10, 0 },
	}

	for direction, coords in pairs(t) do
		clientkeys = gears.table.join(
			clientkeys,
			awful.key(
				{ modkey }, direction, function(c)
					c:relative_move(coords[1], coords[2], 0, 0)
				end,
				{
					group = "Client",
					description = "Move the client " .. direction:lower()
				}
			),
			awful.key(
				{ modkey, "Shift" }, direction, function(c)
					c:relative_move(0, 0, coords[1], coords[2])
				end,
				{
					group = "Client",
					description = "Resize the client " .. direction:lower()
				}
			)
		)
	end
end


local clientbuttons = gears.table.join(
	awful.button({}, 1, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
	end),
	awful.button({ modkey }, 1, function(c)
			c:emit_signal("request::activate", "mouse_click", { raise = true })
			awful.mouse.client.move(c)
	end),
	awful.button({ modkey }, 3, function(c)
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
			placement = awful.placement.no_overlap+awful.placement.no_offscreen
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
				margins = dpi(beautiful.wibar_padding),
				{
					layout = wibox.layout.align.horizontal,
					expand = "none",
					{
						layout = wibox.layout.fixed.horizontal,
						spacing = dpi(10),
						awful.widget.dashboard(),
						awful.widget.taglist_styled(s),
						widgets.layout
					},
					{
						layout = wibox.layout.fixed.horizontal,
						awful.widget.tasklist_styled(s)
					},
					{
						layout = wibox.layout.fixed.horizontal,
						spacing = dpi(10),
						awful.widget.screenshot(),
						widgets.network,
						widgets.volume,
						widgets.battery,
						widgets.date()
					}
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
gears.wallpaper.set(beautiful.wallpaper)
screen.connect_signal("property::geometry", function()
		gears.wallpaper.set(beautiful.wallpaper)
end)


client.connect_signal("manage", function(c)
		-- Disable the border when the layout is the max layout
		if c.first_tag.layout == awful.layout.suit.max then
			c.border_width = 0
		end
		if not awesome.startup then return end
		if c.size_hints.user_position then return end
		if c.size_hints.program_position then return end
		awful.placement.no_offscreen(c)
end)


client.connect_signal("focus", function(c) -- Recolor the border when focused
		c.border_color = beautiful.border_focus
end)


client.connect_signal("unfocus", function(c) -- Recolor the border when unfocused
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
					forced_height = dpi(15),
					forced_width = dpi(15)
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

		awful.titlebar(c, {size = dpi(40)}):setup {
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
				spacing = dpi(10),
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


tag.connect_signal("property::layout", function(t)
		local titlebar_show = false
		local border = 0

		if t.layout == awful.layout.suit.tile.right then
			t.useless_gap = beautiful.useless_gap
			border = beautiful.border_width
		else
			t.useless_gap = 0
		end

		if t.layout == awful.layout.suit.floating then
			titlebar_show = true
		end

		for _, c in ipairs(t:clients()) do
			c.border_width = border
			c.maximized = false
			if titlebar_show then
				awful.titlebar.show(c)
			elseif not c.floating then
				awful.titlebar.hide(c)
			end
		end
end)

local collectgarbage = collectgarbage
gears.timer { -- More frequent garbage collection
	timeout = 30,
	autostart = true,
	callback = collectgarbage
}
