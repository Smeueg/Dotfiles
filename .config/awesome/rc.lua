-- Smeueg's Awesomewm configuration
--
-- TODO:
--   * Add a dropdown menu widget for the wibar
--     (to select screenshot, system, or pick a program)
--   * Create a wallpaper (cube in the middle)
--   * Add a layout widget on the wibar
--


-- Libraries --
-- Make sure LuaRocks packages is loaded if installed
pcall(require, "luarocks.loader")
require("awful.autofocus")
local gears         = require("gears")
local beautiful     = require("beautiful")
local awful         = require("awful")
local wibox         = require("wibox")
local naughty       = require("naughty")
local menubar       = require("menubar")

local hotkeys_popup = require("awful.hotkeys_popup")
local hotkeys_popup = require("awful.hotkeys_popup")
local hotkeys_popup = awful.hotkeys_popup
local cairo         = require("lgi").cairo
local dpi           = beautiful.xresources.apply_dpi



-- Handle errors if there are any --
do
	local in_error = false
	awesome.connect_signal("debug::error",
	function(err)
		if in_error then return end
		in_error = true
		naughty.notify({
				preset = naughty.config.presets.critical,
				title = "Oops, an error happened!",
				text = tostring(err)})
		in_error = false
	end)
end



-- Variables --
local function command_exists(cmd)
	-- Check if a command can be ran
	if cmd == nil then return false end
	for dir in string.gmatch(os.getenv("PATH"), "([^:]+)") do
		if gears.filesystem.file_executable(dir .. "/" .. cmd) then
			return cmd
		end
	end
	return false
end

local home           = os.getenv("HOME")
local terminal       = os.getenv("TERMINAL") or "x-terminal-emulator"
local editor         = os.getenv("EDITOR")
local modkey         = "Mod4"
local screenshot_dir = "/tmp/"
local browser        = command_exists(os.getenv("BROWSER"))
	or command_exists("brave-browser")
	or command_exists("brave")
	or nil


-- Aesthetic Variables (colors & fonts) --
local themes = {
	["Smeueg"] = {
		wallpaper          = home .. "/.config/rice/Shark Space.png",
		yellow             = "#FEA34B",
		red                = "#C5483F",
		green              = "#819013",
		background_dark    = "#291F2E",
		background         = "#322638",
		background_light   = "#35283b",
		background_lighter = "#382B3F",
		foreground         = "#E7DEC7",
		foreground2        = "#493751",
		font               = "JetBrainsMono Nerd Font Mono 11"
	}
}
local theme = themes["Smeueg"]
theme.icon_color = theme.foreground2
theme.menu_color = theme.red


-- Shapes --
local titlebar_circle = gears.shape.transform(gears.shape.circle)
	:scale(0.6, 0.6):translate(3, 7)
local button_close = gears.surface.load_from_shape(
	20, 20, titlebar_circle, theme.red)
local button_maximize = gears.surface.load_from_shape(
	20, 20, titlebar_circle, theme.green)
local button_minimize = gears.surface.load_from_shape(
	20, 20, titlebar_circle, theme.yellow)


-- Theme Variables --
beautiful.init()
beautiful.fg_normal = theme.foreground
beautiful.fg_focus  = beautiful.fg_normal
beautiful.bg_normal	= theme.background
beautiful.font		= theme.font
-- Wibar
beautiful.wibar_selected_tag	= theme.foreground
beautiful.wibar_unselected_tag	= theme.foreground2
-- Titlebar
beautiful.titlebar_close_button_normal				= button_close
beautiful.titlebar_close_button_focus				= button_close
beautiful.titlebar_maximized_button_normal			= button_maximize
beautiful.titlebar_maximized_button_normal_active	= button_maximize
beautiful.titlebar_maximized_button_normal_inactive = button_maximize
beautiful.titlebar_maximized_button_focus			= button_maximize
beautiful.titlebar_maximized_button_focus_active	= button_maximize
beautiful.titlebar_maximized_button_focus_inactive	= button_maximize
beautiful.titlebar_minimize_button_normal			= button_minimize
beautiful.titlebar_minimize_button_focus			= button_minimize
beautiful.titlebar_bg  = theme.background
beautiful.titlebar_fg  = theme.foreground
-- Menu Bar
beautiful.menubar_fg_focus = theme.red
beautiful.menubar_bg_focus = theme.background_lighter
-- Tasklist
beautiful.tasklist_bg_focus		= theme.background_lighter
beautiful.tasklist_bg_normal	= theme.background
beautiful.tasklist_bg_minimize	= beautiful.wibar_bg
-- Borders & Gaps
beautiful.border_normal = theme.background
beautiful.border_focus  = theme.red
beautiful.border_width  = dpi(4)
beautiful.useless_gap   = 5
-- Taglists
beautiful.taglist_bg_focus		= beautiful.wibar_bg
beautiful.taglist_squares_sel	= nil
beautiful.taglist_squares_unsel = nil
-- Notification
beautiful.notification_border_color = theme.red
beautiful.notification_border_width = 3



-- Custom Images/Icons --
local icon_clock = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(icon_clock)
cr:set_source(gears.color(theme.icon_color))
cr:rectangle(11, 5, 1, 6)
cr:rectangle(11, 10, 3, 1)
gears.shape.transform(gears.shape.radial_progress)
	:scale(0.6, 0.6)
	:translate(10, 7)(cr, 20, 20)
cr:fill()

local icon_calendar = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(icon_calendar)
cr:set_source(gears.color(theme.icon_color))
cr:rectangle(7, 4, 12, 12)
cr:stroke()
cr:rectangle(7, 5, 12, 3)
cr:rectangle(9, 9, 2, 2)
cr:rectangle(9, 12, 2, 2)
cr:rectangle(12, 9, 2, 2)
cr:rectangle(12, 12, 2, 2)
cr:rectangle(15, 9, 2, 2)
cr:rectangle(15, 12, 2, 2)
cr:fill()

local icon_volume = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(icon_volume)
cr:set_source(gears.color(theme.icon_color))
gears.shape.transform(gears.shape.rectangle)
	:translate(3, 7.5)(cr, 3, 6)
gears.shape.transform(gears.shape.isosceles_triangle)
	:rotate_at(6, 6, -math.pi/2)
	:translate(-4.5, 2)(cr, 12, 9)
gears.shape.transform(gears.shape.arc)
	:translate(1.8, 4.5)(cr, 12, 12, 1, -math.pi/6, math.pi/6, true, true)
gears.shape.transform(gears.shape.arc)
	:translate(2, 3)(cr, 15, 15, 1, -math.pi/4, math.pi/4, true, true)
gears.shape.transform(gears.shape.arc)
	:translate(2.2, 1.5)(cr, 18, 18, 1, -math.pi/3.5, math.pi/3.5, true, true)
cr:fill()

local icon_mute = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(icon_mute)
cr:set_source(gears.color(theme.icon_color))
gears.shape.transform(gears.shape.rectangle)
	:translate(3, 7.5)(cr, 3, 6)
gears.shape.transform(gears.shape.isosceles_triangle)
	:rotate_at(6, 6, -math.pi/2)
	:translate(-4.5, 2)(cr, 12, 9)
gears.shape.transform(gears.shape.cross)
	:rotate_at(4.5, 4.5, math.pi/4)
	:translate(13, -4)(cr, 9, 9, 1)
cr:fill()

local icon_wifi = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(icon_wifi)
cr:set_source(gears.color(theme.icon_color))
gears.shape.transform(gears.shape.pie)
	:scale(1.25, 1.5)
	:translate(-1.5, 1.5)(cr, 20, 20, 1.25 * math.pi, 1.75* math.pi)
cr:fill()

local icon_ethernet = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(icon_ethernet)
cr:set_source(gears.color(theme.icon_color))
cr:rectangle(9, 2, 6, 6)
cr:rectangle(4, 14, 6, 6)
cr:rectangle(14, 14, 6, 6)
cr:rectangle(11, 7, 2, 4)
cr:rectangle(7, 10, 10, 2)
cr:rectangle(6, 10, 2, 4)
cr:rectangle(16, 10, 2, 4)
cr:fill()

local icon_no_network = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(icon_no_network)
cr:set_source(gears.color(theme.icon_color))
gears.shape.transform(gears.shape.pie)
	:scale(1.15, 1.4)
	:translate(-1.5, 1.8)(cr, 20, 20, 1.25 * math.pi, 1.75* math.pi)
cr:stroke()

local icon_menu = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(icon_menu)
cr:set_source(gears.color(theme.menu_color))
gears.shape.transform(gears.shape.losange)
	:translate(6, 1)(cr, 8, 18)
gears.shape.transform(gears.shape.losange)
	:translate(1, 6)(cr, 18, 8)
cr:fill()
gears.shape.transform(gears.shape.losange)
	:translate(1, 1)(cr, 18, 18)
cr:stroke()



-- Custom Functions --
local saved_gap          = beautiful.useless_gap
local saved_border_width = beautiful.border_width
local function set_layout_all(layout)
	for _, t in pairs(root.tags()) do awful.layout.set(layout, t) end

	if layout == awful.layout.suit.floating then
		for _, c in pairs(client.get()) do awful.titlebar.show(c) end
	else
		for _, c in pairs(client.get()) do
			if not c.floating then
				awful.titlebar.hide(c)
				c.minimized = false
				c.maximized = false
			end
		end
	end

	if layout == awful.layout.suit.tile.right then
		beautiful.useless_gap = saved_gap
		for _, c in pairs(client.get()) do
			c.border_width = beautiful.border_width
		end
	else
		beautiful.useless_gap = 0
		for _, c in pairs(client.get()) do
			c.border_width = 0
		end
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
		for _, c in ipairs(client.get()) do
			if c.class == "Emacs" then
				scratch_client = c
				find_or_spawn_emacs()
				return
			end
		end

		naughty.notify {title = "Opening emacs"}
		awful.spawn.raise_or_spawn(
			"emacs --internal-border=20",
			{},
			nil,
			nil,
			function(c) scratch_client = c; find_or_spawn_emacs() end
		)
	end
end


local function set_wallpaper(s)
	if gears.filesystem.file_readable(theme.wallpaper) then
		gears.wallpaper.maximized(theme.wallpaper, s, false)
	else
		if beautiful.wallpaper then
			local wallpaper = beautiful.wallpaper
			-- If wallpaper is a function, call it with the screen
			if type(wallpaper) == "function" then
				wallpaper = wallpaper(s)
			end
			gears.wallpaper.maximized(wallpaper, s, false)
		end
	end
end


local function load_droidcam_module()
	if command_exists("pactl") then
		awful.spawn.easy_async(
			"ps -C droidcam-cli --no-header -o 'cmd'",
			function(stdout)
				if not stdout:match("-a") then return end
				awful.spawn.easy_async(
					"pactl list short",
					function(stdout)
						if stdout:match("droidcam_audio") then return end
						naughty.notify {title = "Loading Droidcam Audio Module"}
						awful.spawn.easy_async(
							"pactl load-module module-alsa-source device=hw:Loopback,1,0 source_properties=device.description=droidcam_audio",
							function(stdout)
								awful.spawn.easy_async(
									"pactl list short", function(stdout)
										if not stdout:match("droidcam_audio") then return end

										naughty.notify {title = "Successfully loaded droidcam module"}
										awful.spawn("pactl set-default-source 'alsa_input.hw_Loopback_1_0'")
								end)
						end)
					end)
		end)
	end
end


local function toggle_popup(arg)
	if not popup then
		texts = {}
		for i, opt in ipairs(arg) do
			texts[i] = {
				{
					{
						text   = opt[1],
						widget = wibox.widget.textbox
					},
					top    = 15,
					bottom = 15,
					left   = 60,
					right  = 60,
					widget = wibox.container.margin
				},
				id     = "bg",
				shape  = gears.shape.rectangle,
				widget = wibox.container.background
			}
		end
		texts.layout = wibox.layout.align.vertical
		texts.widget = wibox.container.background

		popup = awful.popup {
			widget = {
				{
					{
						texts,
						layout = wibox.layout.align.vertical,
						widget = wibox.container.background
					},
					{
						{
							{
								text   = "Cancel",
								widget = wibox.widget.textbox
							},
							top    = 15,
							bottom = 15,
							left   = 60,
							right  = 60,
							widget = wibox.container.margin
						},
						id     = "bg",
						shape  = gears.shape.rectangle,
						widget = wibox.container.background
					},
					layout = wibox.layout.align.vertical,
					widget = wibox.container.background
				},
				margins = 15,
				widget  = wibox.container.margin,
				update  = function(self)
					local bgs = self:get_children_by_id("bg")
					for i, _ in ipairs(bgs) do bgs[i].bg = theme.background end
					bgs[popup.selected].bg = theme.background_lighter
				end
			},
			border_width	= beautiful.border_width,
			border_color	= beautiful.border_focus,
			placement		= awful.placement.centered,
			visible			= true,
			ontop			= true
		}
		local bgs = popup.widget:get_children_by_id("bg")
		for i, _ in ipairs(bgs) do
			local func = load("popup.selected="..i..";popup.widget:update()")
			bgs[i]:connect_signal("mouse::enter", func)
		end
		popup.widget.widget:connect_signal("button::press", function()
											   popup.ctrl("select") end)
		popup.selected = 1
		popup.opts     = arg
		popup.widget:update()
		popup.keygrabber = awful.keygrabber {
			keybindings = {
				{{ },         "space",  function() popup.ctrl("select") end},
				{{ },         "Return", function() popup.ctrl("select") end},
				{{"Control"}, "j",      function() popup.ctrl("select") end},
				{{ },         "j",      function() popup.ctrl("down") end},
				{{ },         "k",      function() popup.ctrl("up") end},
				{{ },         "Down",   function() popup.ctrl("down") end},
				{{ },         "Up",     function() popup.ctrl("up") end},
				{{"Control"}, "n",      function() popup.ctrl("down") end},
				{{"Control"}, "p",      function() popup.ctrl("up") end},
				{{"Control"}, "g",      function() toggle_popup(nil) end},
				{{"Control"}, "c",      function() toggle_popup(nil) end}
			},
			autostart = true,
		}

		popup.ctrl = function(action)
			local actions = {
				["up"] = function()
					if popup.selected ~= 1 then
						popup.selected = popup.selected - 1
					end
					popup.widget:update()
				end,
				["down"] = function()
					if popup.selected ~= #popup.opts + 1 then
						popup.selected = popup.selected + 1
					end
					popup.widget:update()
				end,
				["select"] = function()
					if popup.selected ~= #popup.opts + 1 then
						local msg = popup.opts[popup.selected][3]
						local cmd = popup.opts[popup.selected][2]
						if msg and popup.opts.msg_after then
							toggle_popup()
							awful.spawn.easy_async_with_shell(
								"sleep 0.1; " .. cmd .. " && printf 'y'",
								function(stdout)
									if stdout ~= 'y\n' then return end
									naughty.notify(msg)
								end
							)
						elseif msg then
							toggle_popup()
							naughty.notify(msg)
							awful.spawn(cmd)
						else
							toggle_popup()
							awful.spawn(cmd)
						end
					else
						toggle_popup()
					end
				end,
			}
			actions[action]()
		end
	else
		popup.keygrabber:stop()
		popup.visible = false
		popup         = nil
	end
end




-- Custom Widgets --
widget_date = wibox.widget {
	{
		{
			{
				{
					image = icon_calendar,
					forced_height = 25,
					forced_width  = 25,
					widget = wibox.widget.imagebox
				},
				right = 7,
				top = 6,
				widget = wibox.container.margin
			},
			{
				format = "%a, %d-%m-%y ",
				refresh = 300,
				widget = wibox.widget.textclock
			},
			layout = wibox.layout.fixed.horizontal,
		},
		bg = theme.background_dark,
		widget = wibox.container.background
	},
	margins = 7,
	widget = wibox.container.margin
}


widget_time = wibox.widget {
	{
		{
			{
				{
					image = icon_clock,
					forced_height = 25,
					forced_width  = 25,
					widget = wibox.widget.imagebox
				},
				right = 7,
				top = 6,
				widget = wibox.container.margin
			},
			{
				format = "%H:%M ",
				refresh = 10,
				widget = wibox.widget.textclock
			},
			layout = wibox.layout.fixed.horizontal,
		},
		bg = theme.background_dark,
		widget = wibox.container.background
	},
	margins = 7,
	widget = wibox.container.margin
}


widget_volume = wibox.widget {
	{
		{
			{
				{
					id            = "icon",
					forced_height = 23,
					forced_width  = 23,
					widget        = wibox.widget.imagebox
				},
				left   = 4,
				right  = 7,
				top    = 7,
				widget = wibox.container.margin
			},
			{
				id     = "vol",
				format = "%H:%M ",
				widget = wibox.widget.textbox
			},
			layout = wibox.layout.fixed.horizontal,
		},
		bg     = theme.background_dark,
		widget = wibox.container.background
	},
	margins = 7,
	widget  = wibox.container.margin,
	buttons = gears.table.join(
		awful.button({ }, 3, function() widget_volume:ctrl("toggle") end),
		awful.button({ }, 4, function() widget_volume:ctrl("+1") end),
		awful.button({ }, 5, function() widget_volume:ctrl("-1") end)
	),

	timer = gears.timer {
		timeout = 5,
		autostart = true,
		callback = function() widget_volume:update() end
	},

	ctrl = function(self, cmd)
		local cmds = {
			["+1"]      = "pactl set-sink-volume @DEFAULT_SINK@ +1%",
			["-1"]      = "pactl set-sink-volume @DEFAULT_SINK@ -1%",
			["+5"]      = "pactl set-sink-volume @DEFAULT_SINK@ +5%",
			["-5"]      = "pactl set-sink-volume @DEFAULT_SINK@ -5%",
			["toggle"]  = "pactl set-sink-mute   @DEFAULT_SINK@ toggle",
			["default"] = "pactl set-sink-volume  @DEFAULT_SINK@ 40%"
		}
		awful.spawn.with_line_callback(
			cmds[cmd],
			{exit = function() widget_volume:update() end})
	end,

	update = function(self)
		awful.spawn.easy_async(
			"pactl list sinks",
			function(stdout)
				self:get_children_by_id("vol")[1].text = stdout:match("Volume:[^%%]+"):match("[0-9]+$") .. "% "
				if stdout:match("Mute: no\n") then
					self:get_children_by_id("icon")[1].image = icon_volume
				else
					self:get_children_by_id("icon")[1].image = icon_mute
				end
		end)

		load_droidcam_module()
	end
}
widget_volume:update()


widget_network = wibox.widget {
	{
		{
			{
				{
					forced_height = 22,
					forced_width  = 22,
					id     = "icon",
					widget = wibox.widget.imagebox
				},
				left   = 5,
				right  = 7,
				top    = 8,
				widget = wibox.container.margin
			},
			{
				format = "%H:%M ",
				id     = "text",
				widget = wibox.widget.textbox
			},
			layout = wibox.layout.fixed.horizontal,
		},
		bg = theme.background_dark,
		widget = wibox.container.background
	},
	margins = 7,
	widget = wibox.container.margin,

	timer = gears.timer {
		timeout = 10,
		autostart = true,
		callback = function() widget_network:update() end
	},

	update = function(self)
		awful.spawn.easy_async(
			"pidof NetworkManager", function(stdout)
				if stdout == "" then return end

				awful.spawn.easy_async(
					"nmcli -t -f name,type connection show --active",
					function(stdout)
						local str = ""
						for net_name, net_type in stdout:gmatch("([^:]+):([^\n]+)") do
							if str ~= "" then str = str .. " " end
							str	= str .. net_name

							if net_type:match("[^-]+$") == "wireless" then
								self:get_children_by_id("icon")[1].image = icon_wifi
							else
								self:get_children_by_id("icon")[1].image = icon_ethernet
							end
						end


						if str ~= "" then
							self:get_children_by_id("text")[1].text	= str .. " "
						else
							self:get_children_by_id("text")[1].text	 = "No Network "
							self:get_children_by_id("icon")[1].image = icon_no_network
						end
					end
				)
			end
		)

		awful.spawn.easy_async(
			"pidof connmand",
			function(stdout)
				if stdout == "" then return end

				awful.spawn.easy_async(
					"connmanctl services", function(stdout)
						local str = ""
						for match in stdout:gmatch("[*]A[A-Za-z] [^ ]*%s*...") do
							if str == "" then
								str = str .. match:match("^... [^ ]*"):match("[^ ]*$")
							else
								str = str .. " "
								str = str .. match:match("^... [^ ]*"):match("[^ ]*$")
							end
							if match:match("wif") then
								self.icon_margin.icon.image = icon_wifi
							elseif match:match("eth")  then
								self.icon_margin.icon.image = icon_ethernet
							end
						end

						if str == "" then
							self.network.text = "Not Connected"
							self.icon_margin.icon.image = icon_no_network
						else
							self.network.text = str
						end
				end)
		end)
	end
}
widget_network:update()


local function system()
	-- Suspend, Shutdown or Reboot the system
	local opts = {
		{"Suspend",  "systemctl suspend",  {title = "Suspending System"}},
		{"Shutdown", "systemctl poweroff", {title = "Powering Off System"}},
		{"Reboot",   "systemctl reboot",   {title = "Rebooting System"}},
		msg_after = false
	}

	toggle_popup(opts)
end

local function screenshot()
	-- Take a screenshot of the screen
	local file_name = screenshot_dir .. os.date("%Y-%m-%d-%H:%M:%S") .. ".png"
	local msg_format = {
		title = "Took a Screenshot",
		text  = "In " .. file_name
	}

	local opts = {
		{"Whole Screen", nil, msg_format},
		{"Region of Screen", nil, msg_format},
		msg_after = true
	}

	if command_exists("scrot") then
		opts[1][2] = "scrot " .. file_name
		opts[2][2] = "scrot -fs " .. file_name
		toggle_popup(opts)
	elseif command_exists("import") then
		opts[1][2] = "import -window root " .. file_name
		opts[2][2] = "import " .. file_name
		toggle_popup(opts)
	else
		naughty.notify {
			title = "Screenshot Tool Not Found",
			text  = "Supported tools are `import from imagemagick` and scrot"
		}
	end
end

local function client_ctrl(key)
	local layout = awful.screen.focused().selected_tag.layout
	local c = client.focus

	if c and c.floating or c and layout == awful.layout.suit.floating then
		local actions = {
			["J"] = function() awful.client.swap.byidx(-1) end,
			["K"] = function() awful.client.swap.byidx(1) end
		}
	else
		local actions = {
			["J"] = function() awful.client.swap.byidx(-1) end,
			["K"] = function() awful.client.swap.byidx(1) end
		}
	end
end

-- Key and Mouse Bindings --
globalkeys = gears.table.join( -- Keybindings
	awful.key({ modkey }, "y", function()
			naughty.notify {title = screen[1].workarea.height}
	end),
	-- Volume
	awful.key({ modkey }, "[", function() widget_volume:ctrl("-5") end),
	awful.key({ modkey }, "]", function() widget_volume:ctrl("+5") end),
	awful.key({ modkey, "Control" }, "[", function()widget_volume:ctrl("-1")end),
	awful.key({ modkey, "Control" }, "]", function()widget_volume:ctrl("+1")end),
	awful.key({ modkey }, "\\", function() widget_volume:ctrl("toggle") end),
	awful.key({ modkey, "Shift" }, "\\", function() widget_volume:ctrl("default") end),
	-- Clients
	awful.key({ modkey }, "j",
		function() awful.client.focus.byidx(-1) end), -- Focus previous window
	awful.key({ modkey }, "k",
		function() awful.client.focus.byidx(1) end), -- Focus previous window
	awful.key({ modkey, "Shift" }, "j",
		function() awful.client.swap.byidx(-1) end), -- Swap with next window
	awful.key({ modkey, "Shift" }, "k",
		function() awful.client.swap.byidx(1) end), -- Swap with next window
	-- Screens
	awful.key({ modkey, "Control" }, "j",
		function() awful.screen.focus_relative(-1) end), -- Focus previous screen
		awful.key({ modkey, "Control" }, "k",
			function() awful.screen.focus_relative(1) end), -- Focus next screen
		-- Layout
		awful.key({ modkey }, "l",
			function() awful.tag.incmwfact(0.05) end), -- Increase master width
		awful.key({ modkey }, "h",
			function() awful.tag.incmwfact(-0.05) end), -- Decrease master width
		awful.key({ modkey }, "t",
			function() set_layout_all(awful.layout.suit.tile.right) end),
		awful.key({ modkey }, "m",
			function() set_layout_all(awful.layout.suit.max) end),
	awful.key({ modkey }, "f",
		function() set_layout_all(awful.layout.suit.floating) end),
	-- Awesome Functions
	awful.key({ modkey, "Control" }, "r", awesome.restart),
	awful.key({ modkey, "Shift" }, "q", awesome.quit),
	-- Menubar
	awful.key({ modkey }, "p", function() menubar.show() end),
	-- Standard programs
	awful.key({ modkey }, "b", function()
			if browser then
				naughty.notify { title = "Opening " .. browser }
				awful.spawn(browser)
			else
				naughty.notify { title = "Error: browser not found" }
			end
	end),
	awful.key({ modkey }, "q", system), -- Suspend, shutdown, or reboot
	awful.key({ modkey }, "s", screenshot)
)


if (terminal == "emacs") then
	globalkeys = gears.table.join(
		globalkeys,
		awful.key({ modkey }, "Return", find_or_spawn_emacs,
			{description = "Move emacs to the current tag or launch emacs",
			 group = "programs"})
	)
else
   globalkeys = gears.table.join(
	  globalkeys,
	  awful.key({ modkey }, "Return", function() awful.spawn(terminal) end,
		 {description = "Launch the terminal", group = "programs"}))
end

clientkeys = gears.table.join( -- Key Bindings That Activate Per-client
    awful.key({ modkey, "Shift" }, "f",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end),
    awful.key({ modkey, "Shift" }, "c", function(c) c:kill() end),
    awful.key({ modkey, "Shift" }, "space",  function(c) c.floating = not c.floating end),
	awful.key({ modkey, "Shift" }, "t", function(c) c.ontop = not c.ontop end),
	awful.key({ modkey }, "space", function(c) c:swap(awful.client.getmaster()) end),
	awful.key({ modkey }, "o",     function(c) c:move_to_screen() end),
	awful.key({ modkey }, "Down",  function(c) c:relative_move(  0,  10,   0, 0) end),
	awful.key({ modkey }, "Up",    function(c) c:relative_move(  0, -10,   0, 0) end),
	awful.key({ modkey }, "Left",  function(c) c:relative_move(-10,   0,   0, 0) end),
		awful.key({ modkey }, "Right", function(c) c:relative_move( 10,   0,   0, 0) end),
		awful.key({ modkey, "Shift" }, "Down",  function(c) c:relative_move(0, 0,   0,  10) end),
		awful.key({ modkey, "Shift" }, "Up",    function(c) c:relative_move(0, 0,   0, -10) end),
		awful.key({ modkey, "Shift" }, "Left",  function(c) c:relative_move(0, 0, -10,   0) end),
		awful.key({ modkey, "Shift" }, "Right", function(c) c:relative_move(0, 0,  10,   0) end)
)

-- Bind Keybindings to Tags
for i = 1, 5 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
					  local tag = awful.screen.focused().tags[i]
					  if tag then tag:view_only() end
                  end),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local tag = awful.screen.focused().tags[i]
                      if tag then awful.tag.viewtoggle(tag) end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then client.focus:move_to_tag(tag) end
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
	{ rule = {},
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

-- Buttons --
clientbuttons = gears.table.join(
	awful.button({ }, 1, function (c)
			c:emit_signal("request::activate", "mouse_click", {raise = true})
								  end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)


-- Signals --
awful.screen.connect_for_each_screen(
	-- Run function for every screen
	function(s)
		set_wallpaper(s)
		awful.tag(
			{"1", "2", "3", "4", "5"},
			s,
			awful.layout.layouts[1]
		)


		-- Create a taglist widget
		s.taglist = awful.widget.taglist {
			screen  = s,
			filter  = awful.widget.taglist.filter.all,
			layout  = {
				spacing = 2,
				layout  = wibox.layout.fixed.horizontal
			},
			widget_template = {
				{
					{
						{
							{
								{
									markup = " ",
									widget = wibox.widget.textbox,
								},
								margins = 3,
								widget  = wibox.container.margin,
							},
							shape              = gears.shape.circle,
							shape_border_width = 1,
							widget             = wibox.container.background,
							id                 = "icon"
						},
						layout = wibox.layout.fixed.horizontal,
					},
					left   = 7,
					right  = 7,
					widget = wibox.container.margin
				},
				id     = 'background_role',
				widget = wibox.container.background,
				create_callback = function(self, t, index, objects)
					self.update_callback(self, t, index, objects)
				end,
				update_callback = function(self, t, index, objects)
					if t.selected then
						self:get_children_by_id("icon")[1].shape_border_color = beautiful.wibar_selected_tag
					else
						self:get_children_by_id("icon")[1].shape_border_color = beautiful.wibar_unselected_tag
					end
					if not next(t:clients()) then
						self:get_children_by_id("icon")[1].bg = beautiful.wibar_bg
					else
						self:get_children_by_id("icon")[1].bg = self:get_children_by_id("icon")[1].shape_border_color
					end
				end
			},
			buttons = gears.table.join(
				awful.button({}, 1, function(t) t:view_only() end),
				awful.button({}, 3, function(t)
						if client.focus then
							client.focus:move_to_tag(t)
						end
				end),
				awful.button({}, 4, function(t)awful.tag.viewnext(t.screen)end),
				awful.button({}, 5, function(t)awful.tag.viewprev(t.screen)end)
			)
		}

		s.mytasklist = awful.widget.tasklist { -- Tasklist Widget
			screen   = s,
			filter   = awful.widget.tasklist.filter.currenttags,
			layout   = {layout = wibox.layout.fixed.horizontal},
			buttons  = awful.button({ }, 1, function(c)
					if   c == client.focus then c.minimized = true
					else c:emit_signal("request::activate", "tasklist",
									   {raise = true}) end; end),
			widget_template = {
				{
					{
						{
							{
								{
									forced_height = 30,
									forced_width  = 30,
									id     = 'icon_role',
									widget = wibox.widget.imagebox,
								},
								top     = 5,
								left    = 5,
								right   = 5,
								widget  = wibox.container.margin,
							},
							layout = wibox.layout.fixed.horizontal,
						},
						id     = 'background_role',
						widget = wibox.container.background,
					},
					margins = 5,
					widget = wibox.container.margin
				},
				bg     = beautiful.wibar_bg,
				widget = wibox.container.background
			}
		}

		s.wibox = awful.wibar { position = "top", screen = s, height = 50 }

		s.wibox:setup { -- Wibox widgets
			layout = wibox.layout.align.horizontal,
			{ -- Left Widgets
				s.taglist,
				layout = wibox.layout.fixed.horizontal
			},
			{ -- Center Widgets
				{widget = wibox.widget.textbox},
				s.mytasklist,
				layout = wibox.layout.flex.horizontal
			},
			{ -- Right Widgets
				widget_network,
				widget_volume,
				widget_date,
				widget_time,
				wibox.widget.textbox(" "),
				spacing = 5,
				layout = wibox.layout.fixed.horizontal
			},
		}
end)
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

client.connect_signal("manage",
function(c)
-- Set the windows at the slave,
-- i.e. put it at the end of others instead of setting
-- it master. If not awesome.startup then
-- awful.client.setslave(c) end
	if awesome.startup
		and not c.size_hints.user_position
		and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after
		-- screen count changes.
		awful.placement.no_offscreen(c)
	end
end)

client.connect_signal("request::titlebars",
function(c)
	-- Add a titlebar when requested (and when titlebars_enabled is true)
	local buttons = gears.table.join(
		awful.button({ }, 1, function()
				c:emit_signal("request::activate", "titlebar", {raise = true})
				awful.mouse.client.move(c)
		end),
		awful.button({ }, 3, function()
				c:emit_signal("request::activate", "titlebar", {raise = true})
				awful.mouse.client.resize(c)
		end)
	)

	awful.titlebar(c, {size = 25}):setup{
		{
			buttons = buttons,
			layout  = wibox.layout.fixed.horizontal
		},
		{
			buttons = buttons,
			layout  = wibox.layout.flex.horizontal
		},
		{ -- Right
			awful.titlebar.widget.minimizebutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal
		},
		layout = wibox.layout.align.horizontal}
end)

client.connect_signal("focus",
function(c) c.border_color = beautiful.border_focus end)

client.connect_signal("unfocus",
function(c) c.border_color = beautiful.border_normal end)

client.connect_signal("property::floating",
function(c)
	if c.floating then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end
end)

client.connect_signal("mouse::enter",
-- Enable sloppy focus, so that focus follows mouse.
function(c)
	c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("manage",
function(c)
	local layout = awful.screen.focused().selected_tag.layout
	if c.floating or layout == awful.layout.suit.floating then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end

	if layout == awful.layout.suit.max then
		c.border_width = dpi(0)
	end
end)


-- Miscellaneous --
gears.timer {
	-- More frequent garbage collecting,
	-- Better clean up those memory leaks
	timeout = 30,
	autostart = true,
	callback = function() collectgarbage() end
}



do  -- Commands to execute in startup
	local function run(cmd)
		local str = cmd:match("^[A-Za-z0-9_-]+")
		if command_exists(str) then
			awful.spawn(cmd)
		else
			local str = "Warning: " .. str .. " is not installed"
			naughty.notify({title = str})
		end
	end
	run("xrandr --output DP-1 --mode 1280x1024 --scale 1.2x1.2")
	run("xset r rate 250 50")
	run("setxkbmap -option keypad:pointerkeys")
	run("xset s off -dpms")

	awful.spawn.with_shell("xrdb ~/.config/X11/Xresources")
	awful.spawn.with_shell("pidof pulseaudio || command -v pulseaudio && setsid --fork pulseaudio --start --exit-idle-time=-1")

	awful.spawn.with_shell("export WINIT_X11_SCALE_FACTOR=1")
	run("pactl set-sink-volume @DEFAULT_SINK@ 40%")
end
