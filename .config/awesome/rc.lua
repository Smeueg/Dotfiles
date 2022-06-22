-- Smeueg's Awesomewm configuration
-- Libraries --
-- Make sure LuaRocks packages is loaded if installed
pcall(require, "luarocks.loader")
require("awful.autofocus")
local gears = require("gears")
local beautiful = require("beautiful")
local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup")
local hotkeys_popup = require("awful.hotkeys_popup")
local hotkeys_popup = awful.hotkeys_popup
local cairo = require("lgi").cairo
local dpi = beautiful.xresources.apply_dpi
-- Personal Modules
require("theme")
local bar = require("bar")
local menu = require("menu")
local dashboard = require("dashboard")
local screenshot = require("screenshot")
local volume = require("volume")

-- Handle errors if there are any --
do
	local in_error = false
	awesome.connect_signal("debug::error", function(err)
		if in_error then
			return
		end
		in_error = true
		naughty.notify({
			title = "Error",
			bg = "#3c3836",
			fg = "#FABD2F",
			text = tostring(err),
		})
		in_error = false
	end)
end

-- Variables --
local home = os.getenv("HOME")
local editor = os.getenv("EDITOR")
local browser = os.getenv("BROWSER")
local terminal = os.getenv("TERMINAL") or "x-terminal-emulator"
local modkey = "Mod4"

-- Custom Functions --
local function command_exists(cmd)
	-- Check if a command can be ran
	if not cmd then
		return false
	end
	for dir in string.gmatch(os.getenv("PATH"), "([^:]+)") do
		if gears.filesystem.file_executable(dir .. "/" .. cmd) then
			return cmd
		end
	end
	return false
end

local function set_layout_all(layout)
	-- Set layout for all tags
	local layouts = awful.layout.suit
	local titlebar_func = awful.titlebar.hide
	local border = beautiful.border_width
	local gap = 0

	if layout == layouts.tile.right then
		gap = beautiful.useless_gap
	elseif layout == layouts.floating then
		titlebar_func = awful.titlebar.show
	elseif layout == layouts.max then
		border = 0
	end

	for _, t in pairs(root.tags()) do
		awful.layout.set(layout, t)
		t.gap = gap
	end

	for _, c in ipairs(client.get()) do
		titlebar_func(c)
		c.border_width = border
	end
end

local function find_or_spawn_emacs()
	-- Spawn Emacs if one is not already set as scratch_client,
	-- else focus and move it to the current tag.
	if scratch_client and scratch_client.valid then
		scratch_client.hidden = false
		scratch_client.minimized = false
		scratch_client:move_to_tag(awful.screen.focused().selected_tag)
		awful.client.focus.byidx(0, scratch_client)
	else -- Spawn emacs and make it the scratchpad client
		for _, c in pairs(client.get()) do
			if c.class == "Emacs" then
				scratch_client = c
				find_or_spawn_emacs()
				return
			end
		end

		naughty.notify({ text = "Opening emacs" })
		awful.spawn.raise_or_spawn("emacs --internal-border=20", {}, nil, nil, function(c)
			scratch_client = c
			find_or_spawn_emacs()
		end)
	end
end

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

local function load_droidcam_module()
	-- A function to load a pulseaudio droidcam module
	if not command_exists("pactl") then
		return
	end
	local module_name = "DroidcamAudio"
	local functions
	functions = {
		{
			"ps -C droidcam-cli --no-header -o 'cmd'",
			function(stdout)
				if stdout:match("-a") then
					awful.spawn.easy_async(functions[2][1], functions[2][2])
				end
			end,
		},
		{
			"pactl list short",
			function(stdout)
				if not stdout:match(module_name) then
					awful.spawn.easy_async(functions[3][1], functions[3][2])
				end
			end,
		},
		{
			"pactl load-module module-alsa-source device=hw:Loopback,1,0 source_properties=device.description="
				.. module_name,
			function(stdout)
				awful.spawn.easy_async(functions[4][1], functions[4][2])
			end,
		},
		{
			"pactl list short",
			function(stdout)
				if stdout:match(module_name) then
					naughty.notify({ text = "Successfully loaded Droidcam module" })
					awful.spawn.with_shell(
						"pactl set-default-source 'alsa_input.hw_Loopback_1_0' && pactl set-source-volume @DEFAULT_SINK@ 150%"
					)
				end
			end,
		},
	}

	awful.spawn.easy_async(functions[1][1], functions[1][2])
end

local function toggle_popup(arg)
	-- A function to either remove the popup or create a new popup
	if popup then
		-- Get rid of popup if one was already made
		popup.widget.keygrabber:stop()
		popup.visible = false
		popup = nil
		return
	end

	-- Create a new popup
	local cols = 1
	local rows = 1
	if arg.orientation == "horizontal" then
		cols = #arg
	elseif arg.orientation == "vertical" then
		rows = #arg
	end
	local template
	local margins = 15
	local options = {
		chosen = 1,
		margins = margins,
		widget = wibox.container.margins,
		forced_num_cols = cols,
		forced_num_rows = rows,
		homogenus = true,
		expand = true,
		layout = wibox.layout.grid,
		select_next = function(self)
			self.chosen = self.chosen + 1
			self:update()
		end,
		select_previous = function(self)
			self.chosen = self.chosen - 1
			self:update()
		end,
		update = function(self)
			-- Update the selected background color
			local options = self:get_children_by_id("option")
			if self.chosen > #options then
				self.chosen = #options
			elseif self.chosen < 1 then
				self.chosen = 1
			end
			for i, option in ipairs(options) do
				option.bg = i == self.chosen and beautiful.menu_bg_normal or beautiful.menu_bg_focus
			end
		end,
		pick = function(self)
			-- Function to run when widget gets clicked
			local func = self:get_children_by_id("option")[self.chosen].func
			self:close()
			if func then
				func()
			end
		end,
		close = function(self)
			toggle_popup()
		end,
	}

	-- Add options from argument
	if arg.cancel then
		table.insert(arg, { text = "Cancel" })
	end
	for i, v in ipairs(arg) do
		if v.text then
			template = {
				text = v.text,
				align = "center",
				widget = wibox.widget.textbox,
			}
		elseif v.image then
			template = {
				image = v.image,
				forced_height = 40,
				forced_width = 40,
				widget = wibox.widget.imagebox,
			}
		end
		table.insert(options, {
			{
				{
					template,
					margins = margins,
					widget = wibox.container.margin,
				},
				id = "option",
				func = v.func,
				widget = wibox.container.background,
			},
			margins = margins,
			widget = wibox.container.margin,
		})
	end
	template = nil
	margins = nil

	-- The Keygrabber (widget keybindigns)
	local keybindings = {
		{
			{ "Control" },
			"n",
			function()
				popup.widget:select_next()
			end,
		},
		{
			{ "Control" },
			"p",
			function()
				popup.widget:select_previous()
			end,
		},
		{
			{ "Control" },
			"g",
			function()
				popup.widget:close()
			end,
		},
		{
			{},
			"q",
			function()
				popup.widget:close()
			end,
		},
		{
			{},
			"Escape",
			function()
				popup.widget:close()
			end,
		},
		{
			{ "Control" },
			"m",
			function()
				popup.widget:pick()
			end,
		},
		{
			{},
			"Return",
			function()
				popup.widget:pick()
			end,
		},
		{
			{},
			" ",
			function()
				popup.widget:pick()
			end,
		},
	}

	if arg.orientation == "horizontal" then
		table.insert(keybindings, {
			{},
			"l",
			function()
				popup.widget:select_next()
			end,
		})
		table.insert(keybindings, {
			{},
			"h",
			function()
				popup.widget:select_previous()
			end,
		})
		table.insert(keybindings, {
			{},
			"Right",
			function()
				popup.widget:select_next()
			end,
		})
		table.insert(keybindings, {
			{},
			"Left",
			function()
				popup.widget:select_previous()
			end,
		})
	else
		table.insert(keybindings, {
			{},
			"j",
			function()
				popup.widget:select_next()
			end,
		})
		table.insert(keybindings, {
			{},
			"k",
			function()
				popup.widget:select_previous()
			end,
		})
		table.insert(keybindings, {
			{},
			"Down",
			function()
				popup.widget:select_next()
			end,
		})
		table.insert(keybindings, {
			{},
			"Up",
			function()
				popup.widget:select_previous()
			end,
		})
	end

	options.keygrabber = awful.keygrabber({
		keybindings = keybindings,
		autostart = true,
	})

	-- Create popup
	popup = awful.popup({
		border_width = 3,
		border_color = beautiful.border_focus,
		placement = awful.placement.centered,
		visible = true,
		ontop = true,
		widget = options,
	})

	-- Add signals to each option
	for i, option in ipairs(popup.widget:get_children_by_id("option")) do
		option:connect_signal("mouse::enter", function()
			popup.widget.chosen = i
			popup.widget:update()
		end)
		option:connect_signal("button::press", function()
			popup.widget:pick()
		end)
	end
	options = nil
	popup.widget:update()
end

local function spawn_browser()
	local AppInfo = require("lgi").Gio.AppInfo
	local app = AppInfo.get_default_for_type("text/html")

	if app then
		naughty.notify({ text = "Opening " .. AppInfo.get_name(app) })
		awful.spawn(AppInfo.get_executable(app))
		return
	end

	naughty.notify({
		title = "Error",
		text = "No default browser found",
	})
end


-- Key and Mouse Bindings --
local globalkeys = gears.table.join( -- Keybindings
	awful.key({ modkey }, "y", function()
			package.loaded["test"] = false
			require("test")
	end),
	-- Volume
	awful.key({ modkey }, "[", function() volume:ctrl("-5") end),
	awful.key({ modkey }, "]", function() volume:ctrl("+5") end),
	awful.key({ modkey }, "\\", function() volume:ctrl("toggle") end),
	awful.key({ modkey, "Control" }, "[", function() volume:ctrl("-1") end),
	awful.key({ modkey, "Control" }, "]", function() volume:ctrl("+1") end),
	awful.key({ modkey, "Shift" }, "\\", function() volume:ctrl("default") end),
	-- Clients
	awful.key({ modkey }, "j", function() -- Focus previous window
		awful.client.focus.byidx(-1)
	end),
	awful.key({ modkey }, "k", function() -- Focus next window
		awful.client.focus.byidx(1)
	end),
	awful.key({ modkey, "Shift" }, "j", function() -- Swap with next window
		awful.client.swap.byidx(-1)
	end),
	awful.key({ modkey, "Shift" }, "k", function() -- Swap with next window
		awful.client.swap.byidx(1)
	end),
	-- Screens
	awful.key({ modkey, "Control" }, "j", function() -- Focus previous screen
		awful.screen.focus_relative(-1)
	end),
	awful.key({ modkey, "Control" }, "k", function() -- Focus next screen
		awful.screen.focus_relative(1)
	end),
	-- Layout
	awful.key({ modkey }, "t", function()
		set_layout_all(awful.layout.suit.tile.right)
	end),
	awful.key({ modkey }, "m", function()
		set_layout_all(awful.layout.suit.max)
	end),
	awful.key({ modkey }, "f", function()
		set_layout_all(awful.layout.suit.floating)
	end),
	awful.key({ modkey }, "l", function() -- Increase master width
		if client.focus and client.focus.first_tag.layout == awful.layout.suit.tile.right then
			awful.tag.incmwfact(0.05)
		end
	end),
	awful.key({ modkey }, "h", function() -- Decrease master width
		local c = client.focus
		if c and c.first_tag.layout == awful.layout.suit.tile.right then
			awful.tag.incmwfact(-0.05)
		end
	end),
	-- Awesome Functions
	awful.key({ modkey, "Control" }, "r", awesome.restart),
	awful.key({ modkey, "Shift" }, "q", awesome.quit),
	awful.key({ modkey, "Shift" }, "b", function()
		local s = awful.screen.focused()
		s.wibox.visible = not s.wibox.visible
	end),
	-- Menubar
	awful.key({ modkey }, "p", menu.open),
	-- Standard programs
	awful.key({ modkey }, "b", spawn_browser),
	awful.key({ modkey }, "Return", function() -- Spawn Emacs
		if terminal == "emacs" then
			find_or_spawn_emacs()
			return
		end
		awful.spawn(terminal)
	end),
	awful.key({ modkey }, "q", dashboard.toggle),
	awful.key({ modkey }, "s", screenshot.toggle)
)

local clientkeys = gears.table.join( -- Key Bindings That Activate Per-client
	awful.key({ modkey, "Shift" }, "f", function(c)
		c.fullscreen = not c.fullscreen
		c:raise()
	end),
	awful.key({ modkey, "Shift" }, "c", function(c)
		c:kill()
	end),
	awful.key({ modkey, "Shift" }, "space", function(c)
		c.floating = not c.floating
	end),
	awful.key({ modkey, "Shift" }, "t", function(c)
		c.ontop = not c.ontop
	end),
	awful.key({ modkey }, "space", function(c)
		c:swap(awful.client.getmaster())
	end),
	awful.key({ modkey }, "o", function(c)
		c:move_to_screen()
	end),
	awful.key({ modkey }, "Down", function(c)
		c:relative_move(0, 10, 0, 0)
	end),
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
	awful.layout.suit.tile,
	awful.layout.suit.max,
	awful.layout.suit.floating,
}

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

-- Buttons --
clientbuttons = gears.table.join(
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

-- Signals --
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
				bar.layout,
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

client.connect_signal("manage", function(c)
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting
	-- it master. If not awesome.startup then
	-- awful.client.setslave(c) end
	if awesome.startup then
		-- Prevent clients from being unreachable after
		-- screen count changes.
		if c.size_hints.user_position then
			return
		end
		if c.size_hints.program_position then
			return
		end
		awful.placement.no_offscreen(c)
	end
end)

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
		c.border_color = beautiful.border_focus
	end
)

client.connect_signal( -- Window border color when not focused
	"unfocus",
	function(c)
		c.border_color = beautiful.border_normal
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

client.connect_signal( -- Enable sloppy focus (window focus follows mouse)
	"mouse::enter",
	function(c)
		c:emit_signal("request::activate", "mouse_enter", { raise = false })
	end
)

client.connect_signal("manage", function(c)
	local layout = awful.screen.focused().selected_tag.layout
	if c.floating or layout == awful.layout.suit.floating then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end

	if layout == awful.layout.suit.max then
		c.border_width = 0
	end
end)

-- Miscellaneous --
gears.timer({ -- More frequent garbage collecting
	timeout = 30,
	autostart = true,
	callback = collectgarbage,
})

do -- Commands to execute in startup
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
			awful.spawn(cmd)
		else
			naughty.notify({ text = "[ERROR]: " .. bin .. " is not installed" })
		end
	end

	-- awful.spawn.with_shell(
	-- 	"pidof pulseaudio ||" ..
	-- 	"command -v pulseaudio &&" ..
	-- 	"setsid --fork pulseaudio --start --exit-idle-time=-1"
	-- )
end
