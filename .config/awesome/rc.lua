-- Smeueg's Awesomewm configuration
-- Libraries --
pcall(require, "luarocks.loader") -- Make sure LuaRocks packages is loaded
require("awful.autofocus")
local gears = require("gears")
local beautiful = require("beautiful")
local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")
local notify = naughty.notify
local lgi = require("lgi")
local cairo = lgi.cairo
local dpi = beautiful.xresources.apply_dpi
-- Other Modules
require("theme")
local bar = require("bar")
local menu = require("menu")
local dashboard = require("dashboard")
local screenshot = require("screenshot")
local volume = require("volume")
local layout = require("layout")
local animate = require("animate")

-- Handle errors if there are any --
do
	local in_error = false
	awesome.connect_signal(
		"debug::error",
		function(err)
			if in_error then return end
			in_error = true
			notify {
				title = "Error",
				bg = "#3c3836",
				fg = "#FABD2F",
				text = tostring(err),
				timeout = 0
			}
			in_error = false
		end
	)
end

-- Variables --
local home = os.getenv("HOME")
local editor = os.getenv("EDITOR")
local browser = os.getenv("BROWSER")
local terminal = os.getenv("TERMINAL")
local modkey = "Mod4"

-- Custom Functions --
local function set_wallpaper(s)
	local file_exists = gears.filesystem.file_readable
	if beautiful.wallpaper and file_exists(beautiful.wallpaper) then
		gears.wallpaper.maximized(beautiful.wallpaper, s, false)
	else
		local tmp = beautiful.wallpaper:match("^#[a-fA-F0-9]+")
		tmp = #tmp
		if not (tmp == 7 or tmp == 4) then return end
		gears.wallpaper.set(beautiful.wallpaper)
	end
end


-- Key and Mouse Bindings --
local globalkeys = gears.table.join( -- Keybindings
	awful.key({ modkey }, "y", function()
			-- For testing purposes only in "./test.lua"
			package.loaded["test"] = false
			require("test")
	end),
	-- Volume
	awful.key(
		{ modkey }, "[", function() volume:ctrl("-5") end,
		{ group = "Volume", description = "Decrease volume by 5%" }
	),
	awful.key(
		{ modkey }, "]", function() volume:ctrl("+5") end,
		{ group = "Volume", description = "Increase volume by 5%" }
	),
	awful.key(
		{ modkey, "Control" }, "[", function() volume:ctrl("-1") end,
		{ group = "Volume", description = "Decrease volume by 1%"}
	),
	awful.key(
		{ modkey, "Control" }, "]", function() volume:ctrl("+1") end,
		{ group = "Volume", description = "Increase volume by 1%" }
	),
	awful.key(
		{ modkey }, "\\", function() volume:ctrl("toggle") end,
		{ group = "Volume", description = "Mute/Unmute" }
	),
	awful.key(
		{ modkey, "Shift" }, "\\", function() volume:ctrl("default") end,
		{ group = "Volume", description = "Set volume to default level" }
	),
	-- Client
	awful.key( -- Focus previous window
		{ modkey }, "j", function() awful.client.focus.byidx(-1) end,
		{ group = "Client", description = "Focus previous client " }
	),
	awful.key( -- Focus next window
		{ modkey }, "k", function() awful.client.focus.byidx(1) end,
		{ group = "Client", description = "Focus next client" }
	),
	awful.key( -- Swap with next window
		{ modkey, "Shift" }, "j", function() awful.client.swap.byidx(-1) end,
		{
			group = "Client",
			description = "Swap the current client with the next client"
		}
	),
	awful.key( -- Swap with next window
		{ modkey, "Shift" }, "k", function() awful.client.swap.byidx(1) end,
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
			layout.set_all(awful.layout.suit.tile.right)
		end,
		{
			group = "Layout",
			description = "Switch to the tiling layout"
		}
	),
	awful.key(
		{ modkey }, "m", function()
			layout.set_all(awful.layout.suit.max)
		end,
		{
			group = "Layout",
			description = "Switch to the tiling layout"
		}
	),
	awful.key(
		{ modkey }, "f", function()
			layout.set_all(awful.layout.suit.floating)
		end,
		{
			group = "Layout",
			description = "Switch to the tiling layout"
		}
	),
	-- Increase master width
	awful.key(
		{ modkey }, "l", function() layout.incmwfact(0.05) end,
		{ group = "Layout", description = "Increase master width factor" }
	),
	-- Decrease master width
	awful.key(
		{ modkey }, "h", function() layout.incmwfact(-0.05) end,
		{ group = "Layout", description = "Decrease master width factor" }
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
	awful.key(
		{ modkey, "Shift" }, "b", function()
			local wibar = awful.screen.focused().wibar
			wibar.visible = not wibar.visible
		end,
		{ group = "Awesome", description = "Toggle bar visibility" }
	),
	-- Applications
	awful.key(
		{ modkey }, "p", menu.open,
		{ group = "Applications" }
	),
	awful.key(
		{ modkey }, "b", function() -- Open the default browser
			local AppInfo = lgi.Gio.AppInfo
			local app = AppInfo.get_default_for_type("text/html")
			if app then
				notify { text = "Opening " .. AppInfo.get_name(app)}
				awful.spawn(AppInfo.get_executable(app))
				return
			end
			notify {
				title = "Error",
				text = "No default browser found",
			}
		end,
		{ group = "Applications", description = "Open the default browser" }
	),
	awful.key(
		{ modkey }, "Return", function() -- Open the terminal
			if terminal == "" then
				notify {title = "Error", text == "$TERMINAL isn't set"}
			elseif terminal == "emacs" then
				notify {text = "Opening emacs"}
				awful.spawn("emacs --internal-border=20")
			else
				awful.spawn(terminal)
			end
		end,
		{ group = "Applications", description = "Open a \"terminal\"" }
	),
	-- Menus/Tools
	awful.key(
		{ modkey }, "q", dashboard.toggle,
		{ group = "Menus/Tools", description = "Open the dashboard" }
	),
	awful.key(
		{ modkey }, "s", screenshot.toggle,
		{ group = "Menus/Tools", description = "Take a screenshot" }
	)
)

local clientkeys = gears.table.join( -- Key Bindings That Activate Per-client
	awful.key({ modkey, "Shift" }, "c", function(c) c:kill() end),
	awful.key({ modkey, "Shift" }, "f", function(c)
			c.floating = not c.floating
	end),
	awful.key({ modkey, "Shift" }, "t", function(c) c.ontop = not c.ontop end),
	awful.key({ modkey }, "o", function(c) c:move_to_screen() end),
	awful.key({ modkey }, "Down", function(c) c:relative_move(0, 10, 0, 0) end),
	awful.key({ modkey }, "Up", function(c)
			c:relative_move(0, -10, 0, 0)
	end),
	awful.key({ modkey }, "Left", function(c)
			c:relative_move(-10, 0, 0, 0)
	end),
	awful.key({ modkey }, "Right", function(c)
		c:relative_move(10, 0, 0, 0)
	end),
	awful.key({ modkey, "Shift" }, "Down", function(c)
		c:relative_move(0, 0, 0, 10)
	end),
	awful.key({ modkey, "Shift" }, "Up", function(c)
		c:relative_move(0, 0, 0, -10)
	end),
	awful.key({ modkey, "Shift" }, "Left", function(c)
		c:relative_move(0, 0, -10, 0)
	end),
	awful.key({ modkey, "Shift" }, "Right", function(c)
		c:relative_move(0, 0, 10, 0)
	end)
)

-- Bind Keybindings to Tags
for i = 1, 5 do
	globalkeys = gears.table.join(
		globalkeys,
		-- View tag only.
		awful.key({ modkey }, "#" .. i + 9, function()
			local tag = awful.screen.focused().tags[i]
			if tag then
				tag:view_only()
			end
		end),
		-- Toggle tag display.
		awful.key({ modkey, "Control" }, "#" .. i + 9, function()
			local tag = awful.screen.focused().tags[i]
			if tag then
				awful.tag.viewtoggle(tag)
			end
		end),
		-- Move client to tag.
		awful.key({ modkey, "Shift" }, "#" .. i + 9, function()
			if not client.focus then
				return
			end
			local tag = client.focus.screen.tags[i]
			if tag then
				client.focus:move_to_tag(tag)
			end
		end)
	)
end
root.keys(globalkeys)

-- Library Variables --
awful.layout.layouts = { -- For layouts
	awful.layout.suit.tile.right,
	awful.layout.suit.max,
	awful.layout.suit.floating,
}

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

awful.rules.rules = { -- Rules
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
			placement = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	},
}


-- Wibar --
awful.screen.connect_for_each_screen(function(s)
	set_wallpaper(s)
	awful.tag({ "1", "2", "3", "4", "5" }, s, awful.layout.layouts[1])
	s.wibar = awful.wibar { position = "top", screen = s, height = 50 }
	s.wibar:setup { -- Wibox widgets
		widget = wibox.container.margin,
		margins = 7.5,
		{
			layout = wibox.layout.align.horizontal,
			expand = "none",
			{ -- Left Widgets
				layout = wibox.layout.fixed.horizontal,
				spacing = 10,
				dashboard.widget,
				bar.taglist_create(s),
				layout.widget,
			},
			{ -- Center Widgets
				layout = wibox.layout.fixed.horizontal,
				spacing = 10,
				bar.tasklist_create(s),
			},
			{ -- Right Widgets
				layout = wibox.layout.fixed.horizontal,
				volume.widget,
				bar.network,
				bar.date,
				spacing = 10,
			},
		}
	}
end)

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

client.connect_signal(
	"manage",
	function(c)
		-- Remove client borders if the current layout is max
		if c.first_tag.layout == awful.layout.suit.max then
			c.border_width = 0
		end
		-- Set the windows at the slave,
		-- i.e. put it at the end of others instead of setting
		-- it master. If not awesome.startup then
		-- awful.client.setslave(c) end
		if not awesome.startup then return end
		-- Prevent clients from being unreachable after
		-- screen count changes.
		if c.size_hints.user_position then return end
		if c.size_hints.program_position then return end
		awful.placement.no_offscreen(c)
	end
)

client.connect_signal("request::titlebars", function(c)
	-- Add a titlebar when requested (and when titlebars_enabled is true)
	local buttons = gears.table.join(
		awful.button({}, 1, function()
			awful.mouse.client.move(c)
			c:emit_signal("request::activate", "titlebar", { raise = true })
		end),
		awful.button({}, 3, function()
			awful.mouse.client.resize(c)
			c:emit_signal("request::activate", "titlebar", { raise = true })
		end)
	)

	local titlebar = awful.titlebar(c, { size = 30 })
	titlebar:setup {
		layout = wibox.layout.align.horizontal,
		{ -- Left
			layout = wibox.layout.fixed.horizontal
		},
		{ -- Center
			layout = wibox.layout.flex.horizontal,
			buttons = buttons
		},
		{ -- Right
			awful.titlebar.widget.minimizebutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal
		}
	}
end)

client.connect_signal( -- Window border color when focused
	"focus",
	function(c)
		animate.color(
			tostring(c),
			0.25,
			c.border_color or beautiful.border_normal,
			beautiful.border_focus,
			function(color) c.border_color = color end
		)
	end
)

client.connect_signal( -- Window border color when not focused
	"unfocus",
	function(c)
		animate.color(
			tostring(c),
			0.25,
			c.border_color or beautiful.border_focus,
			beautiful.border_normal,
			function(color) c.border_color = color end
		)
	end
)

client.connect_signal( -- Enable titlebars when floating
	"property::floating",
	function(c)
		if c.floating then
			awful.titlebar.show(c)
			return
		end
		awful.titlebar.hide(c)
	end
)

-- Miscellaneous --
gears.timer { -- More frequent garbage collecting
	timeout = 30,
	autostart = true,
	callback = collectgarbage,
}

do -- Commands to execute in startup
	local function command_exists(cmd)
		for dir in string.gmatch(os.getenv("PATH"), "([^:]+)") do
			if gears.filesystem.file_executable(dir .. "/" .. cmd) then
				return cmd
			end
		end
		return false
	end

	local cmds = {
		"sct 6000K",
		"xrandr --output DP-1 --mode 1280x1024 --scale 1.3x1.3",
		"xset r rate 250 50",
		"setxkbmap -option keypad:pointerkeys",
		"setxkbmap -option compose:paus",
		"xset s off -dpms",
		"xrdb " .. home .. "/.config/X11/Xresources",
	}

	for _, cmd in pairs(cmds) do
		local bin = cmd:match("^[^ ]+")
		if command_exists(bin) then
			awful.spawn(cmd, false)
		else
			notify { title = "Warning", text = bin .. " isn't installed" }
		end
	end
end
