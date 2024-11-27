--------------------------------------------------------------------------------
--- An Ordinary AwesomeWM Configuration File
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
---
--- Requirements:
--- * pactl (PulseAudio)
--- * nmcli (NetworkManager)
---
--- TODO: Provide a lock screen
--- TODO: Show a popup to display keybindings
--- TODO: Add USEFULL comments
--- TODO: More efficient resource usage
--- TODO: Create a right click menu to kill clients from tasklist using awful.menu
--------------------------------------------------------------------------------
pcall(require, "luarocks.loader") -- Make sure LuaRocks packages is loaded
require("awful.autofocus")
require("libmods")
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
			in_error = false
			notify {
				title = "Error",
				text = tostring(err),
				fg = "#FABD2F",
				timeout = 0
			}
		end
	)
end

-- Theme --
do
	local settings = {}
	-- Default Dark --
	local colors = {
		bg = "#202020",
		red = "#AC4142",
		green = "#90A959",
		yellow = "#F4BF75",
		blue = "#6A9FB5",
		magenta = "#AA759F",
		cyan = "#75B5AA",
		white = "#D0D0D0",
		black = "#202020",
		black_bright = "#222222",
		black_brighter = "#303030",
		accent = "#404040"
	}
	-- Default
	settings.wallpaper = colors.bg
	settings.font = "JetBrainsMono Nerd Font Mono 11"
	settings.bg_normal = colors.black_bright
	settings.fg_normal = colors.white
	settings.icon_color = colors.white
	settings.common_margin = dpi(10)
	-- Windows
	settings.useless_gap = dpi(10)
	settings.border_width = dpi(2)
	settings.border_focus = colors.white
	settings.border_normal = colors.accent
	-- Titlebar
	settings.titlebar_bg = colors.black_brighter
	settings.titlebar_btn_max = colors.green
	settings.titlebar_btn_min = colors.yellow
	settings.titlebar_btn_close = colors.red
	-- Wibar
	settings.wibar_height = dpi(50)
	settings.wibar_padding = dpi(7.5)
	settings.wibar_position = "top"
	settings.wibar_widget_shape = gears.shape.rounded_rect_auto
	settings.wibar_widget_bg = "#00000030"
	-- Popups
	settings.popup_chosen_bg = "#00000030"
	-- Notifications
	settings.notification_border_color = settings.border_focus
	settings.notification_shape = nil
	settings.notification_error = colors.red
	naughty.config.spacing = dpi(5)
	-- Brightness Notification
	settings.brightness_notification_icon_size = dpi(50)
	settings.brightness_notification_delay = 0.5
	-- Tags
	settings.tag_amount = 3
	settings.taglist_fg_focus = colors.white
	settings.taglist_fg_normal = colors.black_brighter
	-- Tasklist
	settings.tasklist_bg_focus = "#00000030"
	settings.tasklist_inner_margin = dpi(5)
	-- Dashboard
	settings.dashboard_margins = dpi(10)
	settings.dashboard_spacing = dpi(1)
	settings.dashboard_text_width = dpi(300)
	settings.dashboard_limit = 10
	settings.dashboard_separator_color = "#ffffff10"
	settings.dashboard_power_icon_size = dpi(25)
	settings.dashboard_launcher_spacing = dpi(5)
	settings.dashboard_search_icon_size = dpi(25)
	settings.dashboard_scrollbar_width = dpi(10)
	settings.dashboard_chosen_bg = "#00000030"
	-- Edge Snapping
	settings.snap_shape = gears.shape.rectangle
	settings.snap_border_width = settings.border_width
	settings.snap_bg = settings.border_focus
	-- UPower/Battery
	settings.battery_warn_threshold = 15
	beautiful.init(settings)
	root.cursor("left_ptr")
end

local pulseaudio = require("system.pulseaudio")
local brightness = require("system.brightness")
local screenshot_popup = require("ui.popup.screenshot")
local dashboard_popup = require("ui.popup.dashboard")
local widgets = require("ui.widgets")
local modkey = "Mod4"


-- Run Commands On Startup --
table.map(
	{
		"setxkbmap -option keypad:pointerkeys -option compose:paus",
		"xrandr --output DP1 --mode 1280x1024 --scale 1.3x1.3",
		"xset r rate 250 50 s off -dpms", -- Set keyboard rate and disable dpms
		"xset b off"                -- Disable "beep" noises
	},
	awful.spawn.if_installed
)

awful.spawn.easy_async( -- Load pulseaudio's bluetooth modules
	string.format("pactl unload-module module-bluetooth-policy module-bluetooth-discover", os.getenv("HOME")),
	function()
		awful.spawn.if_installed("pactl load-module module-bluetooth-policy module-bluetooth-discover")
	end
)

awful.spawn.easy_async(
	string.format("xrdb %s/.config/X11/Xresources", os.getenv("HOME")),
	function()
		awful.spawn.if_installed("xsetroot -cursor_name left_ptr")
	end
)


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
		{
			group = "Layout",
			description = "Increase the number of master windows"
		}
	),
	awful.key(
		{ modkey, "Control" }, "j", function() awful.layout.incnmaster(-1) end,
		{
			group = "Layout",
			description = "Decrease the number of master windows"
		}
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
		{ group = "Application", description = "Launch Emacs" }
	),
	awful.key(
		{ modkey }, "b",
		function()
			local AppInfo = Gio.AppInfo
			local app = AppInfo.get_default_for_type("text/html")
			if app then
				awful.spawn.launch(
					AppInfo.get_name(app),
					AppInfo.get_executable(app)
				)
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
		{ modkey }, "]", function() pulseaudio.modify_vol(5) end,
		{ group = "Volume", description = "Increase The Volume By 5" }
	),
	awful.key(
		{ modkey }, "[", function() pulseaudio.modify_vol(-5) end,
		{ group = "Volume", description = "Decrease The Volume By 5" }
	),
	awful.key(
		{}, "XF86AudioRaiseVolume", function() pulseaudio.modify_vol(5) end,
		{ group = "Volume", description = "Increase The Volume By 5" }
	),
	awful.key(
		{}, "XF86AudioLowerVolume", function() pulseaudio.modify_vol(-5) end,
		{ group = "Volume", description = "Decrease The Volume By 5" }
	),
	awful.key(
		{ modkey, "Control" }, "]", function() pulseaudio.modify_vol(1) end,
		{ group = "Volume", description = "Increase The Volume By 1" }
	),
	awful.key(
		{ modkey, "Control" }, "[", function() pulseaudio.modify_vol(-1) end,
		{ group = "Volume", description = "Decrease The Volume By 1" }
	),
	awful.key(
		{}, "XF86AudioMute", pulseaudio.toggle_mute,
		{ group = "Volume", description = "(Un)Mute Sound" }
	),
	awful.key(
		{ modkey }, "\\", pulseaudio.toggle_mute,
		{ group = "Volume", description = "(Un)Mute Sound" }
	),
	-- Brightness
	awful.key(
		{}, "XF86MonBrightnessUp",
		function() brightness.modify(5) end,
		{
			group = "Brightness",
			description = "Increase the screen's brightness by 5%"
		}
	),
	awful.key(
		{}, "XF86MonBrightnessDown",
		function() brightness.modify(-5) end,
		{
			group = "Brightness",
			description = "Decrease the screen's brightness by 5%"
		}
	),
	awful.key(
		{ "Control" }, "XF86MonBrightnessUp",
		function() brightness.modify(1) end,
		{
			group = "Brightness",
			description = "Increase the screen's brightness by 1%"
		}
	),
	awful.key(
		{ "Control" }, "XF86MonBrightnessDown",
		function() brightness.modify(-1) end,
		{
			group = "Brightness",
			description = "Decrease the screen's brightness by 1%"
		}
	),
	-- Screenshot
	awful.key(
		{ modkey }, "s", screenshot_popup.toggle,
		{ group = "Screenshot", description = "Take A Screenshot" }
	),
	-- Dashboard
	awful.key(
		{ modkey }, "p", dashboard_popup.toggle,
		{ group = "Dashboard", description = "Open The Power Menu" }
	),
	-- Wibar
	awful.key(
		{ modkey, "Shift" }, "b",
		function()
			local wibar = awful.screen.focused().wibar
			wibar.visible = not wibar.visible
		end,
		{ group = "Wibar", description = "Toggle the Wibar" }
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
	),
	awful.key(
		{ modkey, "Shift" }, "t", function(c) c.ontop = not c.ontop end,
		{ group = "Client", description = "Toggle ontop for the client" }
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
			placement = awful.placement.no_overlap + awful.placement.no_offscreen
		}
	}
}



-- Wibar --
awful.screen.connect_for_each_screen(function(s)
		local tags = {}
		for i = 1, beautiful.tag_amount or 5 do tags[i] = tostring(i) end
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
						widgets.dashboard,
						awful.widget.taglist_styled(s),
						widgets.layout
					},
					{
						layout = wibox.layout.fixed.horizontal,
						widgets.tasklist_styled(s)
					},
					{
						layout = wibox.layout.fixed.horizontal,
						spacing = dpi(10),
						widgets.screenshot,
						widgets.network,
						widgets.volume,
						widgets.battery,
						widgets.date
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

-- Recolor the border when focused
client.connect_signal("focus", function(c)
		c.border_color = beautiful.border_focus
end)

-- Recolor the border when unfocused
client.connect_signal("unfocus", function(c)
		c.border_color = beautiful.border_normal
end)

client.connect_signal("manage", function(c)
		-- Get fallback icons
		if c.icon == nil then
			local looped_client = c
			while looped_client.transient_for and looped_client.icon == nil do
				looped_client = looped_client.transient_for
			end

			c.icon = looped_client.icon
		end

		if c.floating then
			c:emit_signal("request::titlebars")
		end

		-- Disable the border when the layout is the max layout
		if c.first_tag.layout == awful.layout.suit.max then
			c.border_width = 0
		end

		if c.size_hints.user_position then return end
		if c.size_hints.program_position then return end
		awful.placement.centered(c)
		awful.placement.no_offscreen(c)
end)


client.connect_signal("property::floating", function(c)
		if c.floating then
			awful.titlebar.show(c)
		else
			awful.titlebar.hide(c)
			if root.tags()[1].layout ~= awful.layout.suit.max then
				c.border_width = beautiful.border_width
			end
		end
end)


local function titlebar_create_btn(btn_color, callback)
	local radius = dpi(15)
	local btn = wibox.widget {
		buttons = awful.button({}, 1, callback),
		widget = wibox.container.background,
		shape = gears.shape.circle,
		bg = btn_color,
		{
			widget = wibox.container.constraint,
			forced_height = radius,
			forced_width = radius
		}
	}

	btn:connect_signal("mouse::enter", function()
			btn.bg = cairo.surface_to_rgba(btn.bg) .. "90"
	end)

	btn:connect_signal("mouse::leave", function()
			btn.bg = cairo.surface_to_rgba(btn.bg)
	end)

	return btn
end

client.connect_signal("request::titlebars", function(c)
		awful.titlebar(c, { size = dpi(40) }):setup {
			layout = wibox.layout.align.horizontal,
			{
				widget = wibox.container.margin,
				margins = dpi(10),
				{
					widget = wibox.widget.imagebox,
					image = c.icon,
				}
			},
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
				titlebar_create_btn(beautiful.titlebar_btn_max, function()
						c.maximized = not c.maximized
				end),
				titlebar_create_btn(beautiful.titlebar_btn_min, function()
						c.minimized = not c.minimized
				end),
				titlebar_create_btn(beautiful.titlebar_btn_close, function()
						c:kill()
				end),
				wibox.widget {}
			}
		}
end)


tag.connect_signal("property::layout", function(t)
		local titlebar_show = false
		local layouts = awful.layout.suit
		local client_border = beautiful.border_width

		if t.layout == layouts.tile.right then
			t.useless_gap = beautiful.useless_gap
		elseif t.layout == layouts.max then
			t.useless_gap = 0
			client_border = 0
		end

		if t.layout == layouts.floating then
			titlebar_show = true
		end

		for _, c in ipairs(t:clients()) do
			c.border_width = client_border
			c.maximized = false
			if titlebar_show then
				c:emit_signal("request::titlebars")
			elseif not c.floating then
				awful.titlebar.hide(c)
			end
		end
end)

gears.timer { -- More frequent garbage collection
	timeout = 30,
	autostart = true,
	callback = function()
		collectgarbage("collect")
	end
}
