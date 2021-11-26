-- TODO (in order)
-- * Add necessary keybindings
-- * Customize statusbar
-- * Customize windowbar
-- * Theme
-- * WIDGETS WOO


-- Libraries --
-- Make sure LuaRocks packages is loaded if installed
pcall(require, "luarocks.loader")
require("awful.autofocus")
local gears			= require("gears")
local beautiful		= require("beautiful")
local awful			= require("awful")
local wibox			= require("wibox")
local naughty		= require("naughty")
local menubar		= require("menubar")
local hotkeys_popup	= require("awful.hotkeys_popup")
local hotkeys_popup	= require("awful.hotkeys_popup")
local hotkeys_popup	= awful.hotkeys_popup
local cairo			= require("lgi").cairo
local dpi			= beautiful.xresources.apply_dpi

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
beautiful.init()
local home          = os.getenv("HOME")
local terminal      = os.getenv("TERMINAL") or "x-terminal-emulator"
local editor        = os.getenv("EDITOR") or "editor"
local modkey        = "Mod4"
local wallpaper     = home .. "/.local/rice/Wallpapers/Retro Galaxy.jpg"
local wallpaper_url = "https://drive.google.com/u/0/uc?id=1yHdTy9CSku8ngkw0ugdj6pNq2UDsiLP0&export=download"


-- Aesthetic Variables (colors & fonts) --
local yellow		= "#EBCB8B"
local red			= "#BF616A"
local green			= "#A3BE8C"
local background	= "#2B303B"
local background2	= "#333945"
local foreground	= "#C0C5CE"
local foreground2	= "#65737E"
local font 			= "JetBrains Mono 11"
local icon_color    = foreground2


-- Shapes --
local titlebar_circle = gears.shape.transform(gears.shape.circle)
	:scale(0.6, 0.6):translate(3, 7)
local button_close = gears.surface.load_from_shape(
	20, 20, titlebar_circle, red)
local button_maximize = gears.surface.load_from_shape(
	20, 20, titlebar_circle, green)
local button_minimize = gears.surface.load_from_shape(
	20, 20, titlebar_circle, yellow)


-- Theme Variables --
beautiful.fg_normal = foreground
beautiful.fg_focus  = fg_normal
beautiful.bg_normal	= background
beautiful.font		= font
-- Wibar
beautiful.wibar_selected_tag	= foreground
beautiful.wibar_unselected_tag	= foreground2
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
beautiful.titlebar_bg  = background
beautiful.titlebar_fg  = foreground
-- Tasklist
beautiful.tasklist_bg_focus		= background2
beautiful.tasklist_bg_normal	= beautiful.wibar_bg
beautiful.tasklist_bg_minimize	= beautiful.wibar_bg
-- Borders & Gaps
beautiful.border_normal = background
beautiful.border_focus  = yellow
beautiful.border_width  = dpi(3)
beautiful.useless_gap   = 5
-- Taglists
beautiful.taglist_bg_focus		= beautiful.wibar_bg
beautiful.taglist_squares_sel	= nil
beautiful.taglist_squares_unsel = nil


-- Custom Images/Icons --
local clock_icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(clock_icon)
cr:set_source(gears.color(icon_color))
cr:rectangle(11, 5, 1, 6)
cr:rectangle(11, 10, 3, 1)
gears.shape.transform(gears.shape.radial_progress)
	:scale(0.6, 0.6)
	:translate(10, 7)(cr, 20, 20)
cr:fill()

local calendar_icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(calendar_icon)
cr:set_source(gears.color(icon_color))
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

local volume_icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(volume_icon)
cr:set_source(gears.color(icon_color))
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

local mute_icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(mute_icon)
cr:set_source(gears.color(icon_color))
gears.shape.transform(gears.shape.rectangle)
	:translate(3, 7.5)(cr, 3, 6)
gears.shape.transform(gears.shape.isosceles_triangle)
	:rotate_at(6, 6, -math.pi/2)
	:translate(-4.5, 2)(cr, 12, 9)
gears.shape.transform(gears.shape.cross)
	:rotate_at(4.5, 4.5, math.pi/4)
	:translate(13, -4)(cr, 9, 9, 1)
cr:fill()

local wifi_icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(wifi_icon)
cr:set_source(gears.color(icon_color))
gears.shape.transform(gears.shape.pie)
	:scale(1.25, 1.5)
	:translate(-1.5, 1.5)(cr, 20, 20, 1.25 * math.pi, 1.75* math.pi)
cr:fill()

local ethernet_icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(ethernet_icon)
cr:set_source(gears.color(icon_color))
cr:rectangle(9, 2, 6, 6)
cr:rectangle(4, 14, 6, 6)
cr:rectangle(14, 14, 6, 6)
cr:rectangle(11, 7, 2, 4)
cr:rectangle(7, 10, 10, 2)
cr:rectangle(6, 10, 2, 4)
cr:rectangle(16, 10, 2, 4)
cr:fill()

local no_network_icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local cr = cairo.Context(no_network_icon)
cr:set_source(gears.color(icon_color))
gears.shape.transform(gears.shape.pie)
	:scale(1.15, 1.4)
	:translate(-1.5, 1.8)(cr, 20, 20, 1.25 * math.pi, 1.75* math.pi)
cr:stroke()





-- Custom Functions --
local saved_gap          = beautiful.useless_gap
local saved_border_width = beautiful.border_width
local function set_layout_all(layout)
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
			c.minimized = false
			c.maximized = false
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

local function command_exists(cmd)
	-- Check if a command can be ran
	for dir in string.gmatch(os.getenv("PATH"), "([^:]+)") do
		if gears.filesystem.file_executable(dir .. "/" .. cmd) then
			return true
		end
	end
	return false
end

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


-- Custom Widgets --
date_widget = wibox.widget {
	{
		{
			image = calendar_icon,
			forced_height = 30,
			forced_width = forced_height,
			widget = wibox.widget.imagebox
		},
		left = 9,
		right = 7,
		top = 9,
		bottom = 9,
		widget = wibox.container.margin
	},
	{
		format = "%a, %d-%m-%y",
		refresh = 300,
		widget = wibox.widget.textclock
	},
	layout = wibox.layout.fixed.horizontal,
	widget = wibox.container.background
}

time_widget = wibox.widget {
	{

		{
			image = clock_icon,
			forced_height = 30,
			forced_width = forced_height,
			widget = wibox.widget.imagebox
		},
		left = 9,
		right = 6,
		top = 9,
		bottom = 9,
		widget = wibox.container.margin
	},
	{
		format = "%H:%M",
		refresh = 10,
		widget = wibox.widget.textclock
	},
	layout = wibox.layout.fixed.horizontal,
	widget = wibox.container.background
}

volume_widget = wibox.widget {
	{
		{
			id = "icon",
			forced_height = 30,
			forced_width = forced_height,
			widget = wibox.widget.imagebox
		},
		id = "icon_margin",
		top = 9,
		bottom = 9,
		left = 9,
		right = 6,
		widget = wibox.container.margin
	},
	{
		id = "vol",
		widget = wibox.widget.textbox
	},
	layout = wibox.layout.fixed.horizontal,
	widget = wibox.container.background,
	update = function(self)
		awful.spawn.easy_async(
			"pactl list sinks", function(stdout)
				str = stdout:match("Mute: [noyes]+%s+Volume:[^\n]*%d+**")
				if str:match("Mute: no") == "Mute: no" then
					self.icon_margin.icon.image = volume_icon
				else
					self.icon_margin.icon.image = mute_icon
				end

				self.vol.text = str:match("%d+%%")
			end
		)
	end
}

volume_timer = gears.timer {
	timeout = 5,
	call_now = true,
	autostart = true,
	callback = function() volume_widget:update() end
}

network_widget = wibox.widget {
	{
		{
			id = "icon",
			forced_height = 30,
			forced_width = forced_height,
			widget = wibox.widget.imagebox
		},
		id = "icon_margin",
		top = 9,
		bottom = 9,
		left = 9,
		right = 6,
		widget = wibox.container.margin
	},
	{
		id = "network",
		widget = wibox.widget.textbox
	},
	layout = wibox.layout.fixed.horizontal,
	widget = wibox.container.background,
	update = function(self)
		awful.spawn.easy_async(
			"pidof connmand", function(stdout)
				if stdout ~= "" then
					awful.spawn.easy_async(
						"connmanctl services", function(stdout)
							local str = ""
							for match in stdout:gmatch("A[A-Za-z] [^ ]* *[we][it][fh]") do
								if str == "" then
									str = str .. match:match(".. [^ ]*"):match("[^ ]*$")
								else
									str = str .. " "
									str = str .. match:match(".. [^ ]*"):match("[^ ]*$")
								end
								if match:match("wif") == "wif" then
									self.icon_margin.icon.image = wifi_icon
								elseif match:match("eth") == "eth" then
									self.icon_margin.icon.image = ethernet_icon
								end
							end

							if str == "" then
								self.network.text = "Not Connected"
								self.icon_margin.icon.image = no_network_icon
							else
								self.network.text = str
							end
						end
					)
				end
		end)
	end
}

network_timer = gears.timer {
	timeout = 10,
	call_now = true,
	autostart = true,
	callback = function() network_widget:update() end
}


-- Key and Mouse Bindings --
globalkeys = gears.table.join( -- Key Bindings
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

clientkeys = gears.table.join( -- Key Bindings That Activate Per-client
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

-- Bind Keybindings to Tags
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

		s.mypromptbox = awful.widget.prompt()

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
		s.mytasklist = awful.widget.tasklist { -- Tasklist Widget
			screen   = s,
			filter   = awful.widget.tasklist.filter.currenttags,
			buttons  = tasklist_buttons,
			layout   = {layout = wibox.layout.fixed.horizontal},
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


		s.wibox = awful.wibar({ position = "top", screen = s, height = 40 })

		s.wibox:setup { -- Wibox widgets
			layout = wibox.layout.align.horizontal,
			{ -- Left Widgets
				layout = wibox.layout.fixed.horizontal,
				s.taglist,
				s.mypromptbox
			},
			{ -- Center Widgets
				{widget = wibox.widget.textbox},
				s.mytasklist,
				layout = wibox.layout.flex.horizontal
			},
			{ -- Right Widgets
				network_widget,
				volume_widget,
				date_widget,
				time_widget,
				{widget = wibox.widget.textbox},
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


client.connect_signal("manage",
function(c)
	if c.floating or c.first_tag.layout == awful.layout.suit.floating then
		awful.titlebar.show(c)
	else
		awful.titlebar.hide(c)
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
