-- TODO
-- Add statusbar (time, wifi, volume, ram)
-- Add a scratchpad
-- Add floating widgets (lock screen, other stuff)
-- Add notifications


-- Import libraries --
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")

pcall(require, "luarocks.loader")
require("awful.autofocus")
require("awful.hotkeys_popup.keys")
-- ---------------- --


-- Fancy Variables --
local bw   = 2 -- Border Width
local bh   = 35 -- Bar Height
local bg   = "#191E1E"
local fg   = "#C5B7AD"
local fg2  = "#3F4B4B"

local occupied_tag = "    "
local empty_tag    = "    "

beautiful.init({
	useless_gap   = 10,
	border_width  = bw,
	border_normal = bg,
	border_focus  = fg,
	border_marked = bg,
	font          = "JetBrainsMono 20"
})
-- ---------- --


-- Variables --
browser  = os.getenv("BROWSER")  or "firefox"
terminal = os.getenv("TERMINAL") or "st"
wallpaper = os.getenv("HOME") .. "/.local/rice/Wallpapers/Rock Thing.png"
modkey = "Mod4"
-- --------- --


-- Wallpaper --
function SetWallpaper()
	if io.open(wallpaper, "r") ~= nil then
		gears.wallpaper.maximized(wallpaper, nil)
	end
end
SetWallpaper()

screen.connect_signal("property::geometry", SetWallpaper)
-- --------- --


-- Layouts --
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.floating,
    awful.layout.suit.max
}
-- ------- --


-- Custom Functions --
local function Volume(action, value)
	cmd = "pactl set-sink-volume @DEFAULT_SINK@ +5%"
	if action == "+" then
		cmd   = "set-sink-volume"
		value = "+" .. value
	elseif action == "-" then
		cmd   = "set-sink-volume"
		value = "-" .. value
	elseif action == "mute" then
		cmd   = "set-sink-mute"
		value = "toggle"
	end

	awful.spawn("pactl " .. cmd .. " @DEFAULT_SINK@ " .. value)
	SetVol()
end


local function SetLayoutAll(layout)
	for _, t in pairs(root.tags()) do
		awful.layout.set(layout, t)
	end
end
-- ---------------- --


-- Keybindings --
globalkeys = gears.table.join(
	awful.key({modkey, "Shift"}, "p", awful.tag.viewprev, {description = "View Previous Tag", group = "Tag"}),
	awful.key({modkey, "Shift"}, "n", awful.tag.viewnext, {description = "View Next Tag", group = "Tag"}),

	awful.key({modkey}, "j", function() awful.client.focus.byidx(-1) end,
		{description = "Focus Next Window (Ordered By Stack/Index)", group = "Window"}
	),

	awful.key({modkey}, "k", function() awful.client.focus.byidx( 1) end,
		{description = "Focus Next Window (Ordered By Stack/Index)", group = "Window"}
	),



	awful.key({modkey}, "bracketright", function() Volume("+", "5%") end,
		{description = "Increase Volume by 5%", group = "Volume"}
	),
	awful.key({modkey}, "bracketleft", function() Volume("-", "5%") end,
		{description = "Decrease Volume by 5%", group = "Volume"}
	),
	awful.key({modkey}, "backslash", function() Volume("mute") end,
		{description = "Mute Audio", group = "Volume"}
	),

	awful.key({modkey, "Shift"}, "j", function() awful.client.swap.byidx(-1) end,
		{description = "Move Window Up The Stack/Index", group = "Window"}),
	awful.key({modkey, "Shift"}, "k", function() awful.client.swap.byidx(1) end,
		{description = "Move Window Down The Stack/Index", group = "Window"}),

	awful.key({modkey, "Control"}, "j", function() awful.screen.focus_relative(1) end,
		{description = "Focus Next Monitor/Screen", group = "Monitor"}),
	awful.key({modkey, "Control"}, "k", function() awful.screen.focus_relative(-1) end,
		{description = "Focus Previous Monitor/Screen", group = "Monitor"}),

	awful.key({modkey, "Control"}, "r", awesome.restart,
		{description = "Restart Awesome", group = "Window Manager"}),
	awful.key({modkey, "Shift"},   "q", awesome.quit,
		{description = "Quit/Exit awesome", group = "Window Manager"}),

	awful.key({modkey, "Shift"}, "l", function() awful.tag.incmwfact( 0.05) end,
		{description = "Increase Master Width Ratio", group = "layout"}),
	awful.key({modkey, "Shift"}, "h", function() awful.tag.incmwfact(-0.05) end,
		{description = "Decrease Master Width Ratio", group = "layout"}),

	awful.key({modkey, "Control"}, "k", function() awful.tag.incnmaster( 1, nil, true) end,
		{description = "Increase The Number of Master Clients", group = "layout"}),
	awful.key({modkey, "Control"}, "j", function() awful.tag.incnmaster(-1, nil, true) end,
		{description = "Decrease The Number of Master Clients", group = "layout"}),

	awful.key({modkey}, "t", function() SetLayoutAll(awful.layout.suit.tile) end,
		{description = "Change Into Tiling Mode", group = "layout"}),
	awful.key({modkey}, "f", function() SetLayoutAll(awful.layout.suit.floating) end,
		{description = "Change Into Floating Mode", group = "layout"}),
	awful.key({modkey}, "m", function() SetLayoutAll(awful.layout.suit.max) end,
		{description = "Change Into Monocle/Maximized Mode", group = "layout"}),

	-- Applications --
	awful.key({modkey}, "Return", function() awful.spawn(terminal .. " -e tmux-spawn") end,
		{description = "Open a Terminal", group = "Applications"}),

	awful.key({modkey}, "b", function()
		awful.spawn(browser)
		naughty.notify({title = "Opened " .. browser})
	end,
		{description = "Open a Web Browser", group = "Applications"}),

	-- Launcher
	awful.key({modkey}, "p", function () awful.screen.focused().launcher:run() end,
		{description = "Launcher", group = "Launcher"})
)

clientkeys = gears.table.join(
    awful.key({modkey}, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "Toggle Fullscreen", group = "Window"}),

    awful.key({modkey, "Shift"}, "c", function(c) c:kill() end,
              {description = "Close", group = "Window"}),

    awful.key({modkey, "Shift"}, "space",  awful.client.floating.toggle,
              {description = "Toggle Floating", group = "Window"}),

    awful.key({modkey}, "space", function(c) c:swap(awful.client.getmaster()) end,
              {description = "Make Window Master", group = "Window"}),

    awful.key({modkey, "Shift"}, "f",
        function(c)
			if c.maximized then
				c.border_width = beautiful.border_width
			else
				c.border_width = 0
			end
            c.maximized = not c.maximized
			c:raise()
        end,
        {description = "Toggle Fullscreen", group = "Window"})
)

-- Tag Keybindings
for i = 1, 4 do
	globalkeys = gears.table.join(globalkeys,
		awful.key({modkey}, "#" .. i + 9,
		function()
			local screen = awful.screen.focused()
			local tag = screen.tags[i]
			if tag then tag:view_only() end
		end,
		{description = "View Tag "..i, group = "tag"}),

		awful.key({modkey, "Control"}, "#" .. i + 9,
		function()
			local screen = awful.screen.focused()
			local tag = screen.tags[i]
			if tag then awful.tag.viewtoggle(tag) end
		end,
		{description = "Toggle Tag " .. i, group = "tag"}),

		awful.key({modkey, "Shift"}, "#" .. i + 9,
		function ()
			if client.focus then
				local tag = client.focus.screen.tags[i]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
		end,
		{description = "Move Client To Tag "..i, group = "tag"})
		)
end

clientbuttons = gears.table.join(
	awful.button({}, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", {raise = true})
	end),
	awful.button({modkey}, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", {raise = true})
	awful.mouse.client.move(c)
	end),
	awful.button({modkey}, 3, function(c)
		c:emit_signal("request::activate", "mouse_click", {raise = true})
	awful.mouse.client.resize(c)
	end)
)

root.keys(globalkeys)
-- ----------- --

-- Mouse Bindings When Cursor is Hovered above Wallpaper --
root.buttons(gears.table.join(
    awful.button({}, 4, awful.tag.viewnext),
    awful.button({}, 5, awful.tag.viewprev)
))
-- ----------------------------------------------------- --



-- Top Bar --
local tags = gears.table.join(
                    awful.button({}, 1, function(t) t:view_only() end),
                    awful.button({}, 2, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({}, 3, awful.tag.viewtoggle),
                    awful.button({}, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({}, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local windows = gears.table.join(
                     awful.button({}, 1, function (c)
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
                     awful.button({}, 2, function(c)
                                              if c == client.focus then
                                                  c.minimized = true
											  end
                                          end),
                     awful.button({}, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({}, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({}, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))


-- Custom Widgets --
local function seperatorWidget(string)
	return wibox.widget {
		text = string,
		widget = wibox.widget.textbox
	}
end

local timeWidget = wibox.widget {
	{
		{
			{
				text = "",
				font = "Material Icons 12",
				widget = wibox.widget.textbox,
			},
			layout = wibox.layout.fixed.horizontal
		},
		fg = fg2,
		widget = wibox.container.background,
	},
	{
		{
			{
				format = ' %R',
				widget = wibox.widget.textclock,
				font   = "JetBrainsMono 11"
			},
			layout = wibox.layout.fixed.horizontal
		},
		fg = fg,
		widget = wibox.container.background,
	},
	layout = wibox.layout.fixed.horizontal
}

local dateWidget = wibox.widget {
	{
		{
			{
				text = "",
				font = "Material Icons 12",
				widget = wibox.widget.textbox,
			},
			layout = wibox.layout.fixed.horizontal
		},
		fg = fg2,
		widget = wibox.container.background,
	},
	{
		{
			{
				format = ' %A, %d-%m-%Y',
				widget = wibox.widget.textclock,
				font   = "JetBrainsMono 11"
			},
			layout = wibox.layout.fixed.horizontal
		},
		fg = fg,
		widget = wibox.container.background,
	},
	layout = wibox.layout.fixed.horizontal
}

local volumeWidget = wibox.widget {
	{
		{
			text   = "",
			id     = "volicon",
			font   = "Material Icons 12",
			widget = wibox.widget.textbox
		},
		id = "submain",
		fg = fg2,
		widget = wibox.container.background
	},
	{
		{
			text   = " 100%",
			id     = "volvalue",
			font   = "JetBrainsMono 11",
			widget = wibox.widget.textbox
		},
		id = "submain",
		fg = fg,
		widget = wibox.container.background
	},
	id = "main",
	layout = wibox.layout.fixed.horizontal,
}

local function SetVol()
	awful.spawn.easy_async("pactl list sinks", function(stdout, stderr, reason, exit_code)
		output = stdout
		volume = string.match(string.match(output, 'left: [0-9]* / *[0-9]*%%'), '[0-9]*%%')
		muted  = string.match(string.match(output, 'Mute: ...?'), 'no')

		if volume == nil then
			volumeWidget.main.submain.volicon.text = ""
			volumeWidget.main.submain.volvalue.text = " "
		else
			if muted ~= nil then
				volumeWidget.main.submain.volicon.text = ""
			else
				volumeWidget.main.submain.volicon.text = ""
			end
			volumeWidget.main.submain.volvalue.text = " " .. volume
		end
	end)
end
-- ------- --



awful.screen.connect_for_each_screen(function(s)
	-- Tags --
    awful.tag({"    ", "    ", "    ", "    "}, s, awful.layout.layouts[1])

    ---- Create a promptbox for each screen
    s.launcher = awful.widget.prompt({
		prompt = "Launch: ",
		bg = bg,
		fg = fg,
	})


	s.wibar = awful.wibar({
		position = "top",
		screen = s,
		bg = bg,
		fg = fg,
		height = bh
	})

    s.wibar:setup {
        layout = wibox.layout.align.horizontal,
		-- Left Widgets --
        {
			layout = wibox.layout.fixed.horizontal,
			awful.widget.taglist {
				screen = s,
				filter = awful.widget.taglist.filter.all,
				buttons = tags,
				style = {
					fg_occupied = fg2,
					fg_empty    = fg2,
					fg_volatile = fg2,
					fg_focus    = fg,
					font        = "Material Icons 12"
				}
			},
        },

		-- Middle Widget --
		nil,

		-- Right Widgets --
        {
			volumeWidget,
			seperatorWidget("  "),
			timeWidget,
			seperatorWidget("  "),
			dateWidget,

			-- Some Extra Padding in the Right
			seperatorWidget(" "),
            layout = wibox.layout.fixed.horizontal,
        },
    }
end)



-- Rules --
awful.rules.rules = {
    {
		rule = {},
		properties = {
			border_width = bw,
			border_color = bg,
			focus = awful.client.focus.filter,
			raise = true,
			keys = clientkeys,
			buttons = clientbuttons,
			screen = awful.screen.preferred,
			placement = awful.placement.no_overlap+awful.placement.no_offscreen,
			maximized = false,
		}
	},
	{
		rule = {floating = true},
		properties = { titlebars = true }
	}
}
-- ----- --


-- Manages new clients, not sure how --
client.connect_signal("manage", function(c)
	for _, t in pairs(root.tags()) do
		if #t:clients() ~= 0 then
			t.name = occupied_tag
		else
			t.name = empty_tag
		end
	end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        awful.placement.no_offscreen(c)
    end
end)

client.connect_signal("unmanage", function(c)
	for _, t in pairs(root.tags()) do
		if #t:clients() == 0 then
			t.name = empty_tag
		end
	end
end)
-- ---------------------- --


-- Other Signals
client.connect_signal("focus",   function(c) c.border_color = fg end)
client.connect_signal("unfocus", function(c) c.border_color = bg end)

screen.connect_signal("arrange", function(s)
    for _, c in pairs(s.clients) do
        if s.selected_tag.layout.name == "max" and not c.floating then
			c:tags()[1].gap = 0
            c.border_width  = 0
        else
			c:tags()[1].gap = beautiful.useless_gap
            c.border_width  = beautiful.border_width
        end
    end
end)


-- Timer --
gears.timer {
    timeout   = 10,
    call_now  = true,
    autostart = true,
    callback  = SetVol
}
-- ----- --
