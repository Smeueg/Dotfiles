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
local menu = require("menu")
local hotkeys_popup = require("awful.hotkeys_popup")
local hotkeys_popup = require("awful.hotkeys_popup")
local hotkeys_popup = awful.hotkeys_popup
local cairo = require("lgi").cairo
local dpi = beautiful.xresources.apply_dpi

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
local screenshot_dir = "/tmp/"

-- Theming --
local themes = {
	["Smeueg"] = {
		["wallpaper"] = "#2D2232",
		["yellow"] = "#FEA34B",
		["red"] = "#C5483F",
		["green"] = "#819013",
		["bg_dark"] = "#00000030",
		["bg"] = "#322638",
		["bg_light"] = "#382B3F",
		["fg"] = "#E7DEC7",
		["fg2"] = "#493751",
		["font"] = "JetBrainsMono Nerd Font Mono 11",
		["focus"] = "red",
		["focus2"] = "yellow",
		["icon_color"] = "bg_light",
	},
	["Gruvbox"] = {
		["wallpaper"] = "#282828",
		["yellow"] = "#FABD2F",
		["red"] = "#FE8019",
		["green"] = "#B8BB26",
		["cyan"] = "#8EC07C",
		["bg_dark"] = "#00000030",
		["bg"] = "#32302f",
		["bg_light"] = "#3c3836",
		["fg"] = "#EBDBB2",
		["fg2"] = "#504945",
		["font"] = "JetBrainsMono Nerd Font Mono 11",
		["focus"] = "red",
		["focus2"] = "yellow",
		["icon_color"] = "bg_light",
	},
}
local theme = themes["Gruvbox"]
theme["icon_color"] = theme[theme["icon_color"]]
theme["focus2"] = theme[theme["focus2"]]
theme["focus"] = theme[theme["focus"]]

-- Shapes --
local titlebar_circle = gears.shape.transform(gears.shape.circle):scale(0.5, 0.5):translate(7, 10)
local button_close = gears.surface.load_from_shape(20, 20, titlebar_circle, theme["red"])

if false then -- Tests
	local cr = cairo.Context(button_close)
	cr:set_source(gears.color(theme["icon_color"]))
	cr:rectangle(11, 5, 10, 6)
	cr:rectangle(11, 10, 3, 1)
	cr:fill()
end

local button_maximize = gears.surface.load_from_shape(20, 20, titlebar_circle, theme["green"])
local button_minimize = gears.surface.load_from_shape(20, 20, titlebar_circle, theme["yellow"])

-- Theme Variables --
beautiful.init()
beautiful.fg_normal = theme["fg"]
beautiful.fg_focus = beautiful.fg_normal
beautiful.bg_normal = theme["bg"]
beautiful.font = theme["font"]
-- Wibar
beautiful.wibar_selected_tag = theme["fg"]
beautiful.wibar_unselected_tag = theme["fg2"]
-- Titlebar
beautiful.titlebar_close_button_normal = button_close
beautiful.titlebar_close_button_focus = button_close
beautiful.titlebar_minimize_button_normal = button_minimize
beautiful.titlebar_minimize_button_focus = button_minimize
beautiful.titlebar_maximized_button_normal = button_maximize
beautiful.titlebar_maximized_button_normal_active = button_maximize
beautiful.titlebar_maximized_button_normal_inactive = button_maximize
beautiful.titlebar_maximized_button_focus = button_maximize
beautiful.titlebar_maximized_button_focus_active = button_maximize
beautiful.titlebar_maximized_button_focus_inactive = button_maximize
beautiful.titlebar_bg = theme["bg_light"]
beautiful.titlebar_bg_focus = theme["fg2"]
beautiful.titlebar_fg = theme["fg"]
beautiful.titlebar_fg_focus = theme["fg"]
-- Prompt
beautiful.prompt_bg_cursor = beautiful.fg_normal
-- beautiful.prompt_fg_cursor = beautiful.fg_normal
-- Tooltips
beautiful.tooltip_bg = theme["bg_light"]
beautiful.tooltip_fg = theme["fg"]
beautiful.tooltip_border_color = theme["focus"]
beautiful.tooltip_border_width = 1
-- Menu Bar
beautiful.menubar_fg_focus = theme["focus"]
beautiful.menubar_bg_focus = theme["bg_light"]
beautiful.menubar_fg_normal = beautiful.fg_normal
-- Tasklist
beautiful.tasklist_bg_focus = theme["bg_dark"]
beautiful.tasklist_bg_normal = theme["bg"]
beautiful.tasklist_bg_minimize = beautiful.wibar_bg
-- Borders
beautiful.border_normal = theme["bg"]
beautiful.border_focus = theme["focus"]
beautiful.border_width = dpi(4)
-- Menu
beautiful.menu_width = 30
beautiful.menu_height = 30
beautiful.menu_bg_normal = theme["bg_dark"]
beautiful.menu_bg_focus = theme["bg"]
beautiful.menu_border_color = theme["fg2"]
beautiful.menu_border_width = 3
-- Gaps
beautiful.useless_gap = 5
-- Taglists
beautiful.taglist_bg_focus = beautiful.wibar_bg
beautiful.taglist_squares_sel = nil
beautiful.taglist_squares_unsel = nil
beautiful.wibar_selected_tag = theme["focus2"]
-- Notification
beautiful.notification_border_color = theme["focus"]
naughty.config.defaults.border_width = 3
naughty.config.defaults.margin = 10
naughty.config.defaults.timout = 3
naughty.config.spacing = 10

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
	for _, t in pairs(root.tags()) do
		awful.layout.set(layout, t)
	end
	if not saved_attr then
		saved_attr = {
			gap = beautiful.useless_gap,
			border_color = beautiful.border_focus,
			border_width = beautiful.border_width,
		}
	end
	-- Floating
	if layout == awful.layout.suit.floating then
		beautiful.border_focus = beautiful.border_normal
		for _, c in pairs(client.get()) do
			awful.titlebar.show(c)
			c.border_color = beautiful.border_normal
		end
	else
		beautiful.border_focus = saved_attr.border_color
		local c = client.focus
		if c then
			c.border_color = beautiful.border_focus
		end
		for _, c in pairs(client.get()) do
			if not c.floating then
				awful.titlebar.hide(c)
			end
			c.minimized = false
			c.maximized = false
		end
	end
	-- Tile
	if layout == awful.layout.suit.tile.right then
		beautiful.useless_gap = saved_attr.gap
		for _, c in pairs(client.get()) do
			c.border_width = saved_attr.border_width
		end
	else
		beautiful.useless_gap = 0
		for _, c in pairs(client.get()) do
			c.border_width = 0
		end
	end
	-- Max
	if layout == awful.layout.suit.max then
		beautiful.border_width = 0
		for _, c in pairs(client.get()) do
			c.border_width = beautiful.border_width
		end
	else
		beautiful.border_width = saved_attr.border_width
		for _, c in pairs(client.get()) do
			c.border_width = beautiful.border_width
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
	elseif gears.filesystem.file_readable(theme["wallpaper"]) then
		gears.wallpaper.maximized(theme["wallpaper"], s, false)
	else
		local tmp = theme["wallpaper"]:match("^#[a-fA-F0-9]+")
		tmp = #tmp
		if not (tmp == 7 or tmp == 4) then
			return
		end
		gears.wallpaper.set(theme["wallpaper"])
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

-- Custom Widgets --
widgets = {}

widgets.layout = wibox.widget({
	{
		{
			id = "icon",
			widget = wibox.widget.imagebox,
		},
		bg = theme["bg_dark"],
		widget = wibox.container.background,
	},
	margins = 10,
	widget = wibox.container.margin,
	buttons = gears.table.join(awful.button({}, 3, function()
		toggle_popup({
			{
				image = widgets.layout.icon.tile,
				func = function()
					set_layout_all(awful.layout.suit.tile.right)
				end,
			},
			{
				image = widgets.layout.icon.max,
				func = function()
					set_layout_all(awful.layout.suit.max)
				end,
			},
			{
				image = widgets.layout.icon.floating,
				func = function()
					set_layout_all(awful.layout.suit.floating)
				end,
			},
			orientation = "horizontal",
			cancel = false,
		})
	end)),
	start = function(self)
		local cr
		local fg = theme["fg2"]
		self.icon = {
			max = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20),
			tile = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20),
			floating = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20),
		}

		local transform = gears.shape.transform
		cr = cairo.Context(self.icon.floating)
		cr:set_source(gears.color(fg))
		transform(gears.shape.rectangle):translate(4.5, 4.5)(cr, 8, 8)
		transform(gears.shape.rectangle):translate(13.5, 7.5)(cr, 2, 8)
		transform(gears.shape.rectangle):translate(7.5, 13.5)(cr, 8, 2)
		cr:fill()

		cr = cairo.Context(self.icon.tile)
		cr:set_source(gears.color(fg))
		transform(gears.shape.rectangle):translate(4.5, 4.5)(cr, 5, 11)
		transform(gears.shape.rectangle):translate(10.5, 4.5)(cr, 5, 5)
		transform(gears.shape.rectangle):translate(10.5, 10.5)(cr, 5, 5)
		cr:fill()

		cr = cairo.Context(self.icon.max)
		cr:set_source(gears.color(fg))
		transform(gears.shape.rectangle):translate(4.5, 4.5)(cr, 11, 11)
		cr:fill()

		local update = function()
			local l = root.tags()[1].layout
			local layout = awful.layout.suit
			local icon = self.icon
			if l == layout.tile.right then
				self:get_children_by_id("icon")[1].image = icon.tile
			elseif l == layout.max then
				self:get_children_by_id("icon")[1].image = icon.max
			elseif l == layout.floating then
				self:get_children_by_id("icon")[1].image = icon.floating
			end
		end

		self:get_children_by_id("icon")[1].image = self.icon.tile
		tag.connect_signal("property::layout", update)
	end,
	switch = function(self)
		local w_root = self
		local margin = 10
		local layouts = awful.layout.suit
		local template = function(image, func)
			return {
				{
					{
						id = "icon",
						image = image,
						forced_height = 40,
						forced_width = 40,
						widget = wibox.widget.imagebox,
					},
					margins = margin,
					widget = wibox.container.margin,
				},
				id = "option",
				func = func,
				widget = wibox.container.background,
			}
		end

		if w_root.popup then
			w_root.popup.widget.keygrabber:stop()
			w_root.popup.visible = false
			w_root.popup = nil
			return
		end

		w_root.popup = awful.popup({
			widget = {
				{
					template(self.icon.tile, function()
						set_layout_all(layouts.tile.right)
					end),
					template(self.icon.max, function()
						set_layout_all(layouts.max)
					end),
					template(self.icon.floating, function()
						set_layout_all(layouts.floating)
					end),
					forced_num_cols = 3,
					forced_num_rows = 1,
					homogenus = true,
					layout = wibox.layout.grid,
				},
				margins = margin,
				widget = wibox.container.margin,

				update = function(self)
					local options = self:get_children_by_id("option")
					if self.chosen > #options then
						self.chosen = #options
					end
					for i, option in ipairs(options) do
						option.bg = i == self.chosen and beautiful.menu_bg_normal or beautiful.menu_bg_focus
					end
				end,

				start = function(self)
					local options = self:get_children_by_id("option")
					local w_child = self
					w_child.chosen = 1

					local ctrl = {
						select_next = function()
							w_child.chosen = w_child.chosen + 1
							w_child:update()
						end,
						select_previous = function()
							if w_child.chosen ~= 1 then
								w_child.chosen = w_child.chosen - 1
							end
							w_child:update()
						end,
						press = function()
							w_child:get_children_by_id("option")[w_child.chosen]:func()
							w_child.keygrabber:stop()
							w_root.popup.visible = false
							w_root.popup = nil
						end,
						stop = function()
							w_child.keygrabber:stop()
							w_root.popup.visible = false
							w_root.popup = nil
						end,
					}

					w_child.keygrabber = awful.keygrabber({
						keybindings = {
							{ {}, "Right", ctrl.select_next },
							{ {}, "Left", ctrl.select_previous },
							{ {}, "l", ctrl.select_next },
							{ {}, "h", ctrl.select_previous },
							{ { "Control" }, "n", ctrl.select_next },
							{ { "Control" }, "p", ctrl.select_previous },
							{ { "Control" }, "g", ctrl.stop },
							{ {}, "q", ctrl.stop },
							{ {}, "Escape", ctrl.stop },
							{ { "Control" }, "m", ctrl.press },
							{ {}, "Return", ctrl.press },
							{ {}, " ", ctrl.press },
						},
						autostart = true,
					})

					for i, option in ipairs(options) do
						local f = {
							enter = function(self)
								w_child.chosen = self.index
								w_child:update()
							end,
							leave = function()
								w_child:update()
							end,
							press = function()
								option:func()
								w_child:update()
								ctrl.stop()
							end,
						}

						option.index = i
						option:connect_signal("mouse::enter", f.enter)
						option:connect_signal("mouse::leave", f.leave)
						option:connect_signal("button::press", f.press)
					end

					w_child:update()
				end,
			},
			placement = awful.placement.centered,
			border_color = beautiful.border_focus,
			border_width = 5,
			ontop = true,
			visible = true,
		})

		self.popup.widget:start()
	end,
})

widgets.date = wibox.widget({
	{
		{
			{
				{
					id = "icon",
					forced_height = 25,
					forced_width = 25,
					widget = wibox.widget.imagebox,
				},
				right = 7,
				top = 6,
				widget = wibox.container.margin,
			},
			{
				format = "%a - %d/%m/%y ",
				refresh = 300,
				widget = wibox.widget.textclock,
			},
			layout = wibox.layout.fixed.horizontal,
		},
		bg = theme["bg_dark"],
		widget = wibox.container.background,
	},
	margins = 7,
	widget = wibox.container.margin,
	start = function(self)
		local icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
		local cr = cairo.Context(icon)
		cr:set_source(gears.color(theme["icon_color"]))
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

		self:get_children_by_id("icon")[1].image = icon
	end,
})

widgets.time = wibox.widget({
	{
		{
			{
				{
					id = "icon",
					forced_height = 25,
					forced_width = 25,
					widget = wibox.widget.imagebox,
				},
				right = 7,
				top = 6,
				widget = wibox.container.margin,
			},
			{
				format = "%H:%M ",
				refresh = 10,
				widget = wibox.widget.textclock,
			},
			layout = wibox.layout.fixed.horizontal,
		},
		bg = theme["bg_dark"],
		widget = wibox.container.background,
	},
	margins = 7,
	widget = wibox.container.margin,
	start = function(self)
		local icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
		local cr = cairo.Context(icon)
		cr:set_source(gears.color(theme["icon_color"]))
		cr:rectangle(11, 5, 1, 6)
		cr:rectangle(11, 10, 3, 1)
		gears.shape.transform(gears.shape.radial_progress):scale(0.6, 0.6):translate(10, 7)(cr, 20, 20)
		cr:fill()

		self:get_children_by_id("icon")[1].image = icon
	end,
})

widgets.volume = wibox.widget({
	widget = wibox.container.margin,
	margins = 7,
	{
		widget = wibox.container.background,
		bg = theme["bg_dark"],
		{
			layout = wibox.layout.fixed.horizontal,
			{
				{
					widget = wibox.widget.imagebox,
					id = "icon",
					forced_height = 23,
					forced_width = 23,
				},
				widget = wibox.container.margin,
				left = 4,
				right = 7,
				top = 7,
			},
			{
				widget = wibox.widget.textbox,
				id = "vol",
			},
		},
	},
	buttons = gears.table.join(
		awful.button({}, 3, function()
			widgets.volume:ctrl("toggle")
		end),
		awful.button({}, 4, function()
			widgets.volume:ctrl("+1")
		end),
		awful.button({}, 5, function()
			widgets.volume:ctrl("-1")
		end)
	),
	default = "35%",
	timer = gears.timer({
		timeout = 5,
		autostart = true,
		callback = function()
			widgets.volume:update()
		end,
	}),
	ctrl = function(self, cmd)
		local cmds = {
			["+1"] = "pactl set-sink-volume  @DEFAULT_SINK@ +1%",
			["-1"] = "pactl set-sink-volume  @DEFAULT_SINK@ -1%",
			["+5"] = "pactl set-sink-volume  @DEFAULT_SINK@ +5%",
			["-5"] = "pactl set-sink-volume  @DEFAULT_SINK@ -5%",
			["toggle"] = "pactl set-sink-mute    @DEFAULT_SINK@ toggle",
			["default"] = "pactl set-sink-volume  @DEFAULT_SINK@ " .. self.default,
		}

		awful.spawn(cmds[cmd])
		self:update()
	end,
	update = function(self)
		awful.spawn.easy_async("pactl list sinks", function(stdout)
			local icon = self.icon.volume
			local vol = stdout:match("(%d+%%)") .. " "

			if stdout:match("Mute: (%w+)") == "yes" then
				icon = self.icon.mute
			end

			self:get_children_by_id("icon")[1].image = icon
			self:get_children_by_id("vol")[1].text = vol
		end)
		load_droidcam_module()
	end,
	start = function(self)
		local cr
		local pi = math.pi
		local transform = gears.shape.transform
		self.icon = {
			mute = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20),
			volume = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20),
		}

		cr = cairo.Context(self.icon.volume)
		cr:set_source(gears.color(theme["icon_color"]))
		transform(gears.shape.rectangle):translate(3, 7.5)(cr, 3, 6)
		transform(gears.shape.isosceles_triangle):rotate_at(6, 6, pi / -2):translate(-4.5, 2)(cr, 12, 9)
		transform(gears.shape.arc):translate(1.8, 4.5)(cr, 12, 12, 2, pi / -6, pi / 6, true, true)
		transform(gears.shape.arc):translate(2, 3)(cr, 15, 15, 2, pi / -4, pi / 4, true, true)
		transform(gears.shape.arc):translate(2.2, 1.5)(cr, 18, 18, 2, pi / -3.5, pi / 3.5, true, true)
		cr:fill()

		cr = cairo.Context(self.icon.mute)
		cr:set_source(gears.color(theme["icon_color"]))
		transform(gears.shape.rectangle):translate(3, 7.5)(cr, 3, 6)
		transform(gears.shape.isosceles_triangle):rotate_at(6, 6, pi / -2):translate(-4.5, 2)(cr, 12, 9)
		transform(gears.shape.cross):rotate_at(4.5, 4.5, pi / 4):translate(13, -4)(cr, 9, 9, 3)
		cr:fill()

		self:update()
	end,
})

widgets.network = wibox.widget({
	widget = wibox.container.margin,
	margins = 7,
	{
		widget = wibox.container.background,
		bg = theme["bg_dark"],
		{
			layout = wibox.layout.fixed.horizontal,
			{
				{
					widget = wibox.widget.imagebox,
					forced_height = 22,
					forced_width = 22,
					id = "icon",
				},
				widget = wibox.container.margin,
				top = 8,
				right = 7,
				left = 5,
			},
			{
				widget = wibox.widget.textbox,
				format = "%H:%M ",
				id = "text",
			},
		},
	},
	timer = gears.timer({
		timeout = 10,
		autostart = true,
		callback = function()
			widgets.network:update()
		end,
	}),
	update = function(self)
		local w = self
		local parse = function(stdout)
			local str = ""
			for net_name, net_type in stdout:gmatch("([^:]+):([^\n]+)") do
				if str ~= "" then
					str = str .. " "
				end
				str = str .. net_name

				if net_type:match("[^-]+$") == "wireless" then
					self:get_children_by_id("icon")[1].image = self.icon.wifi
				else
					self:get_children_by_id("icon")[1].image = self.icon.eth
				end
			end

			if str ~= "" then
				self:get_children_by_id("text")[1].text = str .. " "
			else
				self:get_children_by_id("text")[1].text = "Offline "
				self:get_children_by_id("icon")[1].image = self.icon.disconnect
			end
		end

		awful.spawn.easy_async("pidof NetworkManager", function(stdout)
			if stdout == "" then
				return
			end
			awful.spawn.easy_async("nmcli -t -f name,type connection show --active", parse)
		end)
	end,
	start = function(self)
		local cr
		self.icon = {
			wifi = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20),
			eth = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20),
			disconnect = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20),
		}

		cr = cairo.Context(self.icon.wifi)
		cr:set_source(gears.color(theme["icon_color"]))
		gears.shape.transform(gears.shape.pie)
			:scale(1.25, 1.5)
			:translate(-1.5, 1.5)(cr, 20, 20, 1.25 * math.pi, 1.75 * math.pi)
		cr:fill()

		cr = cairo.Context(self.icon.eth)
		cr:set_source(gears.color(theme["icon_color"]))
		cr:rectangle(9, 2, 6, 6)
		cr:rectangle(4, 14, 6, 6)
		cr:rectangle(14, 14, 6, 6)
		cr:rectangle(11, 7, 2, 4)
		cr:rectangle(7, 10, 10, 2)
		cr:rectangle(6, 10, 2, 4)
		cr:rectangle(16, 10, 2, 4)
		cr:fill()

		cr = cairo.Context(self.icon.disconnect)
		cr:set_source(gears.color(theme["icon_color"]))
		gears.shape.transform(gears.shape.pie)
			:scale(1.15, 1.4)
			:translate(-1.5, 1.8)(cr, 20, 20, 1.25 * math.pi, 1.75 * math.pi)
		cr:stroke()

		self:update()
	end,
})

widgets.dashboard = {
	wibar = wibox.widget({
		widget = wibox.container.margin,
		margins = 7,
		{
			widget = wibox.container.background,
			bg = theme["bg_dark"],
			{
				widget = wibox.container.margin,
				left = 4,
				right = 7,
				top = 7,
				{
					widget = wibox.widget.imagebox,
					id = "icon",
					forced_height = 23,
					forced_width = 23,
				},
			},
		},
	}),
	widgets = {
		name = { -- Username@Hostname
			font = beautiful.font:match("^[^0-9]+") .. "14",
			widget = wibox.widget.textbox,
			id = "widget",
			valign = "center",
			align = "center",
			create = function(self)
				local hostname, user, f
				user = os.getenv("USER")
				f = io.open("/etc/hostname")
				hostname = f:read("*l")
				f:close()
				user = string.format("<span foreground='%s'>%s</span>", beautiful.wibar_selected_tag, user)
				hostname = string.format("<span foreground='%s'>%s</span>", beautiful.border_focus, hostname)
				self.markup = "<b>" .. user .. "@" .. hostname .. "</b>"
			end,
		},
		info = {
			widget = wibox.container.background,
			layout = wibox.layout.grid,
			vertical_spacing = 500,
			forced_num_cols = 2,
			homogeneous = false,
			id = "widget",
			update = function(self)
				-- Remove other widgets
				local rows, cols = self:get_dimension()
				self:remove_widgets_at(1, 1, rows, cols)

				local add = function(label, value)
					self:add(
						wibox.widget({
							font = beautiful.font:match("^[^0-9]+") .. "12",
							widget = wibox.widget.textbox,
							align = "right",
							markup = string.format(
								"  <b><span foreground='%s'>%s</span>:</b> ",
								beautiful.border_focus,
								label
							),
						}),
						wibox.widget({
							font = beautiful.font:match("^[^0-9]+") .. "12",
							markup = string.format(" <b>%s</b>  ", value),
							widget = wibox.widget.textbox,
						})
					)
				end

				do -- Resolution
					local resolution = awful.screen.focused().geometry
					add("Resolution", resolution.width .. "x" .. resolution.height)
				end
				-- Uptime
				if gears.filesystem.file_readable("/proc/uptime") then
					local f = io.open("/proc/uptime", "r")
					local uptime = f:read("*l"):match("^(%d*).")
					f:close()
					local hour = math.floor(uptime / 3600)
					local minute = (math.floor(uptime / 60) - hour * 60)
					add("Uptime", hour .. " hours, " .. minute .. " minutes")
				end
				-- Memory
				if gears.filesystem.file_readable("/proc/meminfo") then
					local f = io.open("/proc/meminfo", "r")
					local total = f:read("*l")
					f:read("*l")
					local used = f:read("*l")
					f:close()
					total = tonumber(total:match("%d+")) / 1e6
					used = total - tonumber(used:match("%d+")) / 1e6
					add("RAM", string.format("%.1fGB / %.1fGB", used, total))
				end

				if gears.filesystem.file_readable("/etc/os-release") then
					local f = io.open("/etc/os-release", "r")
					add("OS", f:read("*l"):match('"(.*)"'))
					f:close()
				end

				if gears.filesystem.file_readable("/proc/version") then
					local f = io.open("/proc/version")
					add("Kernel", f:read("*l"):match("^%w* %w* ([^ ]*)"))
					f:close()
				end
			end,
		},
		power = {
			widget = wibox.container.place,
			id = "widget",
			{
				widget = wibox.container.background,
				{
					widget = wibox.widget.textbox,
					text = "foo",
				},
			},
			create = function(self)
				local surface_create = cairo.ImageSurface.create
				local transform = gears.shape.transform
				local opts = {
					{
						icon = surface_create(cairo.Format.ARGB32, 20, 20),
						func = function()
							naughty.notify({ text = "Suspending System" })
							awful.spawn("systemctl suspend")
						end,
					},
					{
						icon = surface_create(cairo.Format.ARGB32, 20, 20),
						func = function()
							naughty.notify({ text = "Powering Off System" })
							awful.spawn("systemctl poweroff")
						end,
					},
					{
						icon = surface_create(cairo.Format.ARGB32, 20, 20),
						func = function()
							naughty.notify({ text = "Rebooting System" })
							awful.spawn("systemctl reboot")
						end,
					},
				}

				local cr -- Draw Icons
				cr = cairo.Context(opts[1].icon) -- suspend
				cr:set_source(gears.color(beautiful.wibar_selected_tag))
				transform(gears.shape.rounded_bar):translate(3, 3)(cr, 14, 14)
				cr:fill()
				cr:set_operator(cr, cairo.Operator.clear)
				transform(gears.shape.rounded_bar):translate(9, 1)(cr, 11, 11)
				cr:fill()

				local pi = math.pi
				local angle_start = -0.3 * pi
				local angle_end = 1.3 * pi
				cr = cairo.Context(opts[2].icon) -- Shutdown
				cr:set_source(gears.color(beautiful.wibar_selected_tag))
				transform(gears.shape.arc):translate(3, 3)(cr, 14, 14, 2, -0.3 * pi, 1.3 * pi, true, true)
				transform(gears.shape.rounded_bar):translate(9, 3)(cr, 2, 7)
				cr:fill()

				cr = cairo.Context(opts[3].icon) -- Reboot
				cr:set_source(gears.color(beautiful.wibar_selected_tag))
				gears.shape.transform(gears.shape.arc):translate(3, 3)(cr, 14, 14, 2, -0.1 * pi, 1.5 * pi, true, true)
				gears.shape.transform(gears.shape.isosceles_triangle)
					:translate(10, 3)
					:rotate_at(11, 2, -pi / 8)(cr, 7, 7)
				cr:fill()
				cr = nil

				-- Convert icons and functions to it's own widget
				for i, _ in ipairs(opts) do
					opts[i] = {
						widget = wibox.container.margin,
						margins = 10,
						{
							widget = wibox.container.background,
							shape = gears.shape.rectangle,
							func = opts[i].func,
							id = "option",
							{
								widget = wibox.container.margin,
								margins = 5,
								{
									widget = wibox.widget.imagebox,
									image = opts[i].icon,
									forced_height = 30,
									forced_width = 30,
								},
							},
						},
					}
				end

				opts.widget = wibox.container.background
				opts.layout = wibox.layout.fixed.horizontal
				self[1] = opts
			end,
		},
	},
	toggle = function(self)
		if self.popup then
			self.popup.visible = false
			self.keygrabber:stop()
			self.popup = nil
			return
		end

		local separator = {
			forced_height = 0,
			forced_width = 0,
			thickness = 3,
			widget = wibox.widget.separator,
			color = "#ffffff07",
		}

		-- Create widgets
		for _, w in pairs(self.widgets) do
			if w.create then
				w:create()
			end
		end

		self.popup = awful.popup({
			visible = true,
			ontop = true,
			placement = awful.placement.centered,
			border_color = beautiful.border_focus,
			border_width = 4,
			widget = {
				widget = wibox.container.margin,
				margins = 50,
				{
					forced_num_cols = 1,
					expand = true,
					widget = wibox.container.background,
					layout = wibox.layout.grid,
					self.widgets.name,
					separator,
					self.widgets.info,
					separator,
					self.widgets.power,
				},
			},
		})

		-- Update widgets that need updating
		for i, w in ipairs(self.popup.widget:get_children_by_id("widget")) do
			if w.update then
				w:update(self.widgets)
			end
		end

		-- Mouse Controls for buttons
		for i, w in ipairs(self.popup.widget:get_children_by_id("option")) do
			w:connect_signal("button::press", function()
				self:press()
			end)
			w:connect_signal("mouse::enter", function()
				self.chosen = i
				self:update()
			end)
		end

		-- Keyboard Controls for buttons
		self.keygrabber = awful.keygrabber({
			autostart = true,
			keybindings = {
				{
					{ modkey },
					"q",
					function()
						self:toggle()
					end,
				},
				{
					{ "Control" },
					"g",
					function()
						self:toggle()
					end,
				},
				{
					{},
					"q",
					function()
						self:toggle()
					end,
				},
				{
					{},
					"Escape",
					function()
						self:toggle()
					end,
				},
				{
					{ "Control" },
					"m",
					function()
						self:press()
					end,
				},
				{
					{},
					"Return",
					function()
						self:press()
					end,
				},
				{
					{},
					" ",
					function()
						self:press()
					end,
				},
				{
					{ "Control" },
					"n",
					function()
						self:select_next()
					end,
				},
				{
					{ "Control" },
					"p",
					function()
						self:select_prev()
					end,
				},
				{
					{},
					"l",
					function()
						self:select_next()
					end,
				},
				{
					{},
					"h",
					function()
						self:select_prev()
					end,
				},
				{
					{},
					"Right",
					function()
						self:select_next()
					end,
				},
				{
					{},
					"Left",
					function()
						self:select_prev()
					end,
				},
			},
		})

		self.chosen = 1
		self:update()
	end,
	update = function(self)
		local widgets = self.popup.widget:get_children_by_id("option")
		for _, w in ipairs(widgets) do
			w.bg = nil
		end
		if not self.chosen then
			self.chosen = 1
		end
		if self.chosen > #widgets then
			self.chosen = #widgets
		end
		if self.chosen < 1 then
			self.chosen = 1
		end
		widgets[self.chosen].bg = "#00000020"
	end,
	press = function(self)
		self.popup.widget:get_children_by_id("option")[self.chosen]:func()
		self:toggle()
	end,
	select_next = function(self)
		self.chosen = self.chosen + 1
		self:update()
	end,
	select_prev = function(self)
		self.chosen = self.chosen - 1
		self:update()
	end,
	start = function(self)
		local icon = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
		local cr = cairo.Context(icon)

		cr:set_source(gears.color(beautiful.wibar_unselected_tag))
		gears.shape.transform(gears.shape.rectangle):translate(4, 3)(cr, 15, 3)
		gears.shape.transform(gears.shape.rectangle):translate(4, 9)(cr, 15, 3)
		gears.shape.transform(gears.shape.rectangle):translate(4, 15)(cr, 15, 3)
		cr:fill()

		local buttons = gears.table.join(awful.button({}, 1, function()
			self:toggle()
		end))
		self.wibar:get_children_by_id("icon")[1].image = icon
		self.wibar:get_children_by_id("icon")[1]:buttons(buttons)
	end,
}

widgets.screenshot = {
	screenshot = function(self, method)
		local cmd = {}
		local file = string.format("/tmp/Screenshot - %s.png", os.date("%Y-%m-%d - %H:%M:%S"))
		if command_exists("scrot") then
			cmd = {
				["whole"] = "scrot",
				["partial"] = "scrot -fs",
			}
		elseif command_exists("import") then
			cmd = {
				["whole"] = "import -window root",
				["partial"] = "import",
			}
		else
			naughty.notify({
				title = "No Screenshot Tool Found",
				text = "Supported tools are 'scrot' and 'import' from imagemagick",
			})
			return
		end

		awful.spawn.easy_async_with_shell(
			string.format("sleep 0.05; %s '%s'", cmd[method], file),
			function(stdout, stderr, reason, exit_code)
				if exit_code ~= 0 then
					return
				end
				naughty.notify({
					title = "Took Screenshot",
					text = "As " .. file,
				})
			end
		)
	end,
	update = function(self)
		local widgets = self.popup.widget:get_children_by_id("bg")
		for _, w in ipairs(widgets) do
			w.bg = nil
		end
		if self.chosen > #widgets then
			self.chosen = #widgets
		end
		if self.chosen < 1 then
			self.chosen = 1
		end
		widgets[self.chosen].bg = "#ffffff10"
	end,
	press = function(self)
		local widgets = self.popup.widget:get_children_by_id("bg")
		self.popup.visible = false
		if widgets[self.chosen].func then
			widgets[self.chosen]:func()
		end
		self:toggle()
	end,
	select_next = function(self)
		self.chosen = self.chosen + 1
		self:update()
	end,
	select_prev = function(self)
		self.chosen = self.chosen - 1
		self:update()
	end,
	toggle = function(self)
		if self.popup then
			self.keygrabber:stop()
			self.popup.visible = false
			self.popup = nil
			return
		end

		local template = function(text, func)
			return {
				widget = wibox.container.background,
				func = func,
				id = "bg",
				{
					widget = wibox.container.margin,
					margins = 30,
					{
						widget = wibox.widget.textbox,
						align = "center",
						text = text,
					},
				},
			}
		end

		self.popup = awful.popup({
			border_color = beautiful.border_focus,
			border_width = 4,
			placement = awful.placement.centered,
			visible = true,
			ontop = true,
			widget = {
				widget = wibox.container.margin,
				margins = 15,
				{
					forced_num_cols = 1,
					homogeneous = true,
					layout = wibox.layout.grid,
					expand = true,
					template("Whole Screen", function()
						self:screenshot("whole")
					end),
					template("Partial", function()
						self:screenshot("partial")
					end),
					template("Cancel", nil),
				},
			},
		})

		-- Mouse Support for widgets
		for i, w in ipairs(self.popup.widget:get_children_by_id("bg")) do
			w:connect_signal("button::press", function()
				self:press()
			end)
			w:connect_signal("mouse::enter", function()
				self.chosen = i
				self:update()
			end)
		end

		-- Keyboard controls
		self.keygrabber = awful.keygrabber({
			autostart = true,
			keybindings = {
				{
					{ modkey },
					"s",
					function()
						self:toggle()
					end,
				},
				{
					{ "Control" },
					"g",
					function()
						self:toggle()
					end,
				},
				{
					{},
					"q",
					function()
						self:toggle()
					end,
				},
				{
					{},
					"Escape",
					function()
						self:toggle()
					end,
				},
				{
					{ "Control" },
					"m",
					function()
						self:press()
					end,
				},
				{
					{},
					"Return",
					function()
						self:press()
					end,
				},
				{
					{},
					" ",
					function()
						self:press()
					end,
				},
				{
					{ "Control" },
					"n",
					function()
						self:select_next()
					end,
				},
				{
					{ "Control" },
					"p",
					function()
						self:select_prev()
					end,
				},
				{
					{},
					"j",
					function()
						self:select_next()
					end,
				},
				{
					{},
					"k",
					function()
						self:select_prev()
					end,
				},
				{
					{},
					"Down",
					function()
						self:select_next()
					end,
				},
				{
					{},
					"Up",
					function()
						self:select_prev()
					end,
				},
			},
		})

		self.chosen = 1
		self:update()
	end,
}

for _, w in pairs(widgets) do
	if type(w.start) == "function" then
		w:start()
	end
end

-- Key and Mouse Bindings --
local globalkeys = gears.table.join( -- Keybindings
	awful.key({ modkey }, "y", function()
		package.loaded["test"] = false
		require("test")
	end),
	-- Volume
	awful.key({ modkey }, "[", function()
		widgets.volume:ctrl("-5")
	end),
	awful.key({ modkey }, "]", function()
		widgets.volume:ctrl("+5")
	end),
	awful.key({ modkey }, "\\", function()
		widgets.volume:ctrl("toggle")
	end),
	awful.key({ modkey, "Control" }, "[", function()
		widgets.volume:ctrl("-1")
	end),
	awful.key({ modkey, "Control" }, "]", function()
		widgets.volume:ctrl("+1")
	end),
	awful.key({ modkey, "Shift" }, "\\", function()
		widgets.volume:ctrl("default")
	end),
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
	awful.key({ modkey }, "q", function()
		widgets.dashboard:toggle()
	end),
	awful.key({ modkey }, "s", function()
		widgets.screenshot:toggle()
	end)
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

	-- Create a taglist widget
	s.taglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.all,
		layout = { spacing = 2, layout = wibox.layout.fixed.horizontal },
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
							widget = wibox.container.margin,
						},
						shape = gears.shape.circle,
						shape_border_width = 2,
						widget = wibox.container.background,
						id = "icon",
					},
					layout = wibox.layout.fixed.horizontal,
				},
				left = 7,
				right = 7,
				widget = wibox.container.margin,
			},
			id = "background_role",
			widget = wibox.container.background,
			create_callback = function(self, t, index, objects)
				self:update_callback(t, index, objects)
			end,

			update_callback = function(self, t, index, objects)
				local icon = self:get_children_by_id("icon")[1]
				if t.selected then
					icon.shape_border_color = beautiful.wibar_selected_tag
				else
					icon.shape_border_color = beautiful.wibar_unselected_tag
				end

				if not next(t:clients()) then
					icon.bg = beautiful.wibar_bg
				else
					icon.bg = self:get_children_by_id("icon")[1].shape_border_color
				end
			end,
		},
		buttons = gears.table.join(
			awful.button({}, 1, function(t)
				t:view_only()
			end),
			awful.button({}, 3, function(t)
				local c = client.focus
				if c then
					c:move_to_tag(t)
				end
			end),
			awful.button({}, 4, function(t)
				awful.tag.viewprev(t.screen)
			end),
			awful.button({}, 5, function(t)
				awful.tag.viewnext(t.screen)
			end)
		),
	})

	s.tasklist = awful.widget.tasklist({ -- Tasklist Widget
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		layout = { layout = wibox.layout.fixed.horizontal },
		buttons = gears.table.join(
			awful.button({}, 1, function(c)
				if c == client.focus then
					c.minimized = true
				else
					c:emit_signal("request::activate", "tasklist", { raise = true })
				end
			end),
			awful.button({}, 4, function()
				awful.client.focus.byidx(-1)
			end),
			awful.button({}, 5, function()
				awful.client.focus.byidx(1)
			end)
		),
		widget_template = {
			{
				{
					{
						{
							{
								forced_height = 30,
								forced_width = 30,
								id = "icon_role",
								widget = wibox.widget.imagebox,
							},
							top = 5,
							left = 5,
							right = 5,
							widget = wibox.container.margin,
						},
						layout = wibox.layout.fixed.horizontal,
					},
					id = "background_role",
					widget = wibox.container.background,
				},
				margins = 5,
				widget = wibox.container.margin,
			},
			bg = beautiful.wibar_bg,
			widget = wibox.container.background,
		},
	})

	s.wibox = awful.wibar({ position = "top", screen = s, height = 50 })
	s.layoutbox = awful.widget.layoutbox(s)

	s.wibox:setup({ -- Wibox widgets
		layout = wibox.layout.align.horizontal,
		expand = "none",
		{ -- Left Widgets
			widgets.dashboard.wibar,
			s.taglist,
			widgets.layout,
			layout = wibox.layout.fixed.horizontal,
		},
		{ -- Center Widgets
			s.tasklist,
			layout = wibox.layout.fixed.horizontal,
		},
		{ -- Right Widgets
			widgets.network,
			widgets.volume,
			widgets.date,
			widgets.time,
			wibox.widget.textbox(" "),
			spacing = 5,
			layout = wibox.layout.fixed.horizontal,
		},
	})
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
	titlebar:setup({
		{
			buttons = buttons,
			layout = wibox.layout.fixed.horizontal,
		},
		{
			{
				{ -- Title
					{ widget = wibox.container.background },
					opacity = 0.1,
					bg = "#000000",
					id = "line_color",
					shape = gears.shape.hexagon,
					widget = wibox.container.background,
				},
				buttons = buttons,
				margins = 10,
				widget = wibox.container.margin,
			},
			{
				{ -- Title
					{ widget = wibox.container.background },
					opacity = 0.1,
					bg = "#000000",
					id = "line_color",
					shape = gears.shape.hexagon,
					widget = wibox.container.background,
				},
				buttons = buttons,
				margins = 10,
				widget = wibox.container.margin,
			},
			margins = 10,
			widget = wibox.container.margin,
			layout = wibox.layout.flex.horizontal,
		},
		{ -- Right
			awful.titlebar.widget.minimizebutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal,
		},
		layout = wibox.layout.align.horizontal,
	})
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
		"pactl set-sink-volume @DEFAULT_SINK@ " .. widgets.volume.default,
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
