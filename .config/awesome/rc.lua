-- TODO (in order)
-- * Add necessary keybindings
-- * Customize statusbar
-- * Customize windowbar
-- * Theme
-- * WIDGETS WOO

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")


local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
local cairo = require("lgi").cairo
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local has_fdo, freedesktop = pcall(require, "freedesktop")

-- Variables
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
local home          = os.getenv("HOME")
local terminal      = os.getenv("TERMINAL") or "x-terminal-emulator"
local editor        = os.getenv("EDITOR") or "editor"
local editor_cmd    = terminal .. " -e " .. editor
local wallpaper     = home .. "/.local/rice/Wallpapers/Retro Galaxy.jpg"
local wallpaper_url = "https://drive.google.com/u/0/uc?id=1yHdTy9CSku8ngkw0ugdj6pNq2UDsiLP0&export=download"



local square = gears.shape.transform(gears.shape.rectangle)
	:scale(0.9, 0.9):translate(1,2)

local yellow = "#EBCB8B"
local background = "#2B303B"
local background2 = "#373C46"
local foreground = "#C0C5CE"
local foreground2 = "#65737E"

local titlebar_circle = gears.shape.transform(gears.shape.circle)
	:scale(0.6, 0.6):translate(3, 7)
local button_close = gears.surface.load_from_shape(
	20, 20, titlebar_circle, "#BF616A")
local button_maximize = gears.surface.load_from_shape(
	20, 20, titlebar_circle, "#A3BE8C")
local button_minimize = gears.surface.load_from_shape(
	20, 20, titlebar_circle, yellow)
beautiful.wibar_selected_tag	= foreground
beautiful.wibar_unselected_tag	= foreground2
beautiful.wibar_bg				= background
beautiful.taglist_bg_focus		= beautiful.wibar_bg
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

beautiful.titlebar_bg  = background
beautiful.titlebar_fg  = foreground
beautiful.border_width = dpi(3)
beautiful.useless_gap  = 5
beautiful.font         = "JetBrains Mono 11"

beautiful.tasklist_bg_focus = background2
beautiful.tasklist_bg_normal = beautiful.wibar_bg

beautiful.border_normal = background
beautiful.border_focus  = yellow


local clock_icon = gears.shape.transform(gears.shape.radial_progress)
	:scale(0.6, 0.6):translate(11, 7)
local clock_icon = gears.surface.load_from_shape(
	20,
	20,
	clock_icon,
	foreground2)

local cr = cairo.Context(clock_icon)
cr:set_source(gears.color(foreground2))
cr:rectangle(12, 5, 1, 6)
cr:rectangle(12, 10, 3, 1)
cr:fill()


local calendar_icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(calendar_icon)
cr:set_source(gears.color(foreground2))
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


beautiful.taglist_squares_sel = nil
beautiful.taglist_squares_unsel = nil

-- Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop

     if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end



modkey = "Mod4"

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.max,
    awful.layout.suit.floating,
}

-- Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

local menu_awesome = { "awesome", myawesomemenu, beautiful.awesome_icon }
local menu_terminal = { "open terminal", terminal }



-- Custom Functions --
saved_gap = beautiful.useless_gap
saved_border_width = beautiful.border_width
function set_layout_all(layout)
	for _, t in pairs(root.tags()) do
		awful.layout.set(layout, t)
	end

	if layout == awful.layout.suit.floating then
		for _, c in pairs(client.get()) do
			awful.titlebar.show(c)
		end
	else
		for _, c in pairs(client.get()) do
			awful.titlebar.hide(c)
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

function find_or_spawn_emacs()
   -- Find emacs if found, move it to the current tag and focus.
   -- Else, spawn emacs
   for _, c in ipairs(client.get()) do
	  if (c.class == "Emacs") then
		 c:move_to_tag(awful.screen.focused().selected_tag)
		 awful.client.focus.byidx(0, c)
		 return
	  end
   end

   awful.spawn("emacs")
end

function command_exists(cmd)
	path = os.getenv("PATH")
	for dir in string.gmatch(path, "([^:]+)") do
		if gears.filesystem.file_executable(dir .. "/" .. cmd) then
			return true
		end
	end
	return false
end
----------------------



-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}


-- Create a textclock widget
widget_date = wibox.widget {
	{
		{
			image = calendar_icon,
			forced_height = 30,
			forced_width = forced_height,
			widget = wibox.widget.imagebox
		},
		margins = 5,
		widget = wibox.container.margin
	},
	{
		format = "%a, %d-%m-%y",
		widget = wibox.widget.textclock
	},
	layout = wibox.layout.fixed.horizontal,
	widget = wibox.container.background
}

widget_time = wibox.widget {
	{
		{
			image = clock_icon,
			forced_height = 30,
			forced_width = forced_height,
			widget = wibox.widget.imagebox
		},
		margins = 5,
		widget = wibox.container.margin
	},
	{
		format = "%H:%M",
		widget = wibox.widget.textclock
	},
	layout = wibox.layout.fixed.horizontal,
	widget = wibox.container.background
}

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
	awful.button({ }, 1, function(t) t:view_only() end),
	awful.button({ modkey }, 1, function(t)
			if client.focus then
				client.focus:move_to_tag(t)
			end
	end),
	awful.button({ }, 3, awful.tag.viewtoggle),
	awful.button({ modkey }, 3, function(t)
			if client.focus then
				client.focus:toggle_tag(t)
			end
	end),
	awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
	awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
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
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
	if gears.filesystem.file_readable(wallpaper) then
		gears.wallpaper.maximized(wallpaper, s, true)
	else
		if beautiful.wallpaper then
			local wallpaper = beautiful.wallpaper
			-- If wallpaper is a function, call it with the screen
			if type(wallpaper) == "function" then
				wallpaper = wallpaper(s)
			end
			gears.wallpaper.maximized(wallpaper, s, true)
		end
	end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)


-- Widget Definition --
local volume_widget, volume_timer = awful.widget.watch('bar-volume', 5)


awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)
	awful.tag(
		{"1", "2", "3", "4", "5"},
		s,
		awful.layout.layouts[1]
	)

	-- Create a promptbox for each screen
	s.mypromptbox = awful.widget.prompt()
	-- Create an imagebox widget which will contain an icon indicating which layout we're using.
	-- We need one layoutbox per screen.
	s.mylayoutbox = awful.widget.layoutbox(s)
	s.mylayoutbox:buttons(gears.table.join(
							  awful.button({ }, 1, function() awful.layout.inc( 1) end),
							  awful.button({ }, 3, function() awful.layout.inc(-1) end),
							  awful.button({ }, 4, function() awful.layout.inc( 1) end),
							  awful.button({ }, 5, function() awful.layout.inc(-1) end)))

	-- Create a taglist widget
	s.taglist = awful.widget.taglist {
		screen  = s,
		filter  = awful.widget.taglist.filter.all,
		layout   = {
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
						shape  = gears.shape.circle,
						shape_border_width = 1,
						shape_border_color = "#ffffff",
						widget = wibox.container.background,
						id = "icon"
					},
					layout = wibox.layout.fixed.horizontal,
				},
				left  = 7,
				right = 7,
				widget = wibox.container.margin
			},
			id     = 'background_role',
			widget = wibox.container.background,
			create_callback = function(self, t, index, objects)
				if t.selected then
					self:get_children_by_id("icon")[1].shape_border_color = beautiful.wibar_selected_tag
				else
					self:get_children_by_id("icon")[1].shape_border_color = beautiful.wibar_unselected_tag
				end
				if next(t:clients()) == nil then
					self:get_children_by_id("icon")[1].bg = beautiful.wibar_bg
				else
					self:get_children_by_id("icon")[1].bg = self:get_children_by_id("icon")[1].shape_border_color
				end
			end,
			update_callback = function(self, t, index, objects)
				if t.selected then
					self:get_children_by_id("icon")[1].shape_border_color = beautiful.wibar_selected_tag
				else
					self:get_children_by_id("icon")[1].shape_border_color = beautiful.wibar_unselected_tag
				end
				if next(t:clients()) == nil then
					self:get_children_by_id("icon")[1].bg = beautiful.wibar_bg
				else
					self:get_children_by_id("icon")[1].bg = self:get_children_by_id("icon")[1].shape_border_color
				end
			end
		},
		buttons = taglist_buttons
	}
	-- Create a tasklist widget
	s.mytasklist = awful.widget.tasklist {
		screen   = s,
		filter   = awful.widget.tasklist.filter.currenttags,
		buttons  = tasklist_buttons,
		layout   = {layout = wibox.layout.fixed.horizontal},
		-- Notice that there is *NO* wibox.wibox prefix, it is a template,
		-- not a widget instance.
		widget_template = {
			{
				{
					{
						{
							id     = 'icon_role',
							widget = wibox.widget.imagebox,
						},
						margins = 5,
						widget  = wibox.container.margin,
					},
					layout = wibox.layout.fixed.horizontal,
				},
				left = 5,
				right = 5,
				widget = wibox.container.margin
			},
			id     = 'background_role',
			widget = wibox.container.background,
		},
	}


	-- Create the wibox
	s.mywibox = awful.wibar({ position = "top", screen = s, height = 40 })

	-- Add widgets to the wibox
	s.mywibox:setup {
		layout = wibox.layout.align.horizontal,
		{ -- Left Widgets
			layout = wibox.layout.fixed.horizontal,
			s.taglist,
			s.mypromptbox,
		},
		{
			{
				widget = wibox.widget.textbox
			},
			s.mytasklist,
			layout = wibox.layout.flex.horizontal
		},
		{ -- Right Widgets
			layout = wibox.layout.fixed.horizontal,
			wibox.widget.systray(),
			volume_widget,
			widget_date,
			widget_time,
		},
	}
end)

-- Key bindings
globalkeys = gears.table.join(
	-- Pulse audio volume control
	awful.key({ modkey }, "[", function()
			awful.spawn.with_line_callback(
				"pactl set-sink-volume @DEFAULT_SINK@ -5%",
				{exit = function() volume_timer:emit_signal("timeout") end})
	end, {description = "Decrease volume by 5%", group = "volume"}),
	awful.key({ modkey }, "]", function()
			awful.spawn.with_line_callback(
				"pactl set-sink-volume @DEFAULT_SINK@ +5%",
				{exit = function() volume_timer:emit_signal("timeout") end})
	end, {description = "Increase volume by 5%", group = "volume"}),
	awful.key({ modkey, "Control" }, "[", function()
			awful.spawn.with_line_callback(
				"pactl set-sink-volume @DEFAULT_SINK@ -1%",
				{exit = function() volume_timer:emit_signal("timeout") end})
	end, {description = "Lower volume by 1%", group = "volume"}),
	awful.key({ modkey, "Control" }, "]", function()
			awful.spawn.with_line_callback(
				"pactl set-sink-volume @DEFAULT_SINK@ +1%",
				{exit = function() volume_timer:emit_signal("timeout") end})
	end, {description = "Increase volume by 1%", group = "volume"}),
	awful.key({ modkey }, "\\", function()
			awful.spawn.with_line_callback(
				"pactl set-sink-mute @DEFAULT_SINK@ toggle",
				{exit = function() volume_timer:emit_signal("timeout") end})
	end, {description = "Toggle mute", group = "volume"}),
	-- Clients
	awful.key({ modkey }, "j", function() awful.client.focus.byidx(-1) end,
		{description = "focus next by index", group = "client"}),
	awful.key({ modkey }, "k", function() awful.client.focus.byidx(1) end,
		{description = "focus previous by index", group = "client"}),
	awful.key({ modkey, "Shift" }, "j", function()
			awful.client.swap.byidx(-1) end,
		{description = "swap with previous client by index", group = "client"}),
	awful.key({ modkey, "Shift" }, "k", function()
			awful.client.swap.byidx(1) end,
		{description = "swap with next client by index", group = "client"}),
	-- Screens
	awful.key({ modkey, "Control" }, "j", function()
			awful.screen.focus_relative(-1) end,
		{description = "focus the previous screen", group = "screen"}),
	awful.key({ modkey, "Control" }, "k", function()
			awful.screen.focus_relative(1) end,
		{description = "focus the next screen", group = "screen"}),
	-- Layout
	awful.key({ modkey }, "l", function() awful.tag.incmwfact(0.05) end,
	  {description = "Increase master width factor", group = "layout"}),
   awful.key({ modkey }, "h", function() awful.tag.incmwfact(-0.05) end,
	  {description = "Decrease master width factor", group = "layout"}),
   awful.key({ modkey }, "t", function()
		 set_layout_all(awful.layout.suit.tile.right) end,
	  {description = "Set to tiling layout for all tags", group = "layout"}),
   awful.key({ modkey }, "m", function()
		 set_layout_all(awful.layout.suit.max) end,
	  {description = "Set to monocle layout for all tags", group = "layout"}),
   awful.key({ modkey }, "f", function()
		 set_layout_all(awful.layout.suit.floating) end,
	  {description = "Set to floating layout for all tags", group = "layout"}),
   -- Standard program
   awful.key({ modkey, "Control" }, "r", awesome.restart,
	  {description = "reload awesome", group = "awesome"}),
   awful.key({ modkey, "Shift" }, "q", awesome.quit,
	  {description = "quit awesome", group = "awesome"}),
   -- Prompt
   awful.key({ modkey }, "r", function()
		 awful.screen.focused().mypromptbox:run() end,
	  {description = "run prompt", group = "launcher"}),
   -- Menubar
   awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})
)
if (terminal == "emacs") then
   globalkeys = gears.table.join(
	  globalkeys,
	  awful.key({ modkey }, "Return", find_or_spawn_emacs,
		 {description = "Move emacs to the current tag or launch emacs",
		  group = "programs"}))
else
   globalkeys = gears.table.join(
	  globalkeys,
	  awful.key({ modkey }, "Return", function() awful.spawn(terminal) end,
		 {description = "Launch the terminal", group = "programs"}))
end

-- Keys Per Client
clientkeys = gears.table.join(
    awful.key({ modkey, "Shift" }, "f",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift" }, "c", function(c) c:kill() end,
              {description = "Close window", group = "client"}),
    awful.key({ modkey, "Shift" }, "space",  awful.client.floating.toggle,
              {description = "Toggle floating", group = "client"}),
    awful.key({ modkey }, "space", function(c)
		  c:swap(awful.client.getmaster()) end,
              {description = "Move window to master", group = "client"}),
    awful.key({ modkey }, "o", function(c)
		  c:move_to_screen() end,
	   {description = "Move window to current screen",
		group = "client"}),
    awful.key({ modkey, "Shift" }, "t", function(c) c.ontop = not c.ontop end,
              {description = "toggle keep on top", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 5 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

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

-- Set keys
root.keys(globalkeys)
-- }}}

-- Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
					 size_hints_honor = false,
					 focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "xtightvncviewer"},

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
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
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

client.connect_signal("property::floating", function(c)
	if c.floating then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end
end)

-- Hook called when a client spawns
client.connect_signal("manage", function(c)
	if c.floating or c.first_tag.layout == awful.layout.suit.floating then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
	end
end)
