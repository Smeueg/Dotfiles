--------------------------------------------------------------------------------
--- A popup module for a dashboard
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local beautiful = require("beautiful")
local notify = require("naughty").notify
local gears = require("gears")
local wibox = require("wibox")
local awful = require("awful")
local icons = require("ui.icons")
local lgi = require("lgi")
local cairo = lgi.cairo
local Gio = lgi.Gio
local Gtk = lgi.require("Gtk", "3.0")
local GLib = lgi.GLib
local default_icon_theme = Gtk.IconTheme.get_default()
local Gdk = lgi.Gdk

local dashboard = {}

--- Cache all the needed and available icons
local function cache_icon()
	for _, appinfo in ipairs(Gio.AppInfo.get_all()) do
		if appinfo:should_show() then
			local icon = appinfo:get_icon()
			if icon ~= nil then
				local icon_name = icon:to_string()
				local icon_path = default_icon_theme:lookup_icon(
					icon_name,
					64,
					0
				):get_filename()
				gears.surface.load(icon_path)
			end
		end
	end
end

--- Execute a keybinding in Awesome
---@param modifiers string[]|nil
---@param key string
local function execute_keybinding(modifiers, key)
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

---@class PowerOpt
---@field action string
local PowerOpt = {}
local PowerOpt_mt = { __index = PowerOpt }

--- Creates an option for the power widget
---@param icon image
---@param action string
---@return PowerOpt
function PowerOpt:new(icon, action)
	local opt = wibox.widget {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect_auto,
		action = action,
		{
			widget = wibox.container.margin,
			margins = beautiful.dashboard_margins,
			{
				widget = wibox.widget.imagebox,
				image = icon,
				forced_height = beautiful.dashboard_power_icon_size,
				forced_width = beautiful.dashboard_power_icon_size
			}
		}
	}
	return setmetatable(opt, PowerOpt_mt)
end

--- Changes the background color for the PowerOpt
function PowerOpt:highlight()
	self:set_bg(beautiful.dashboard_chosen_bg)
end

--- Returns the background color to it's original color for the PowerOpt
function PowerOpt:unhighlight()
	self:set_bg(nil)
end

--- Runs the action of a power option
function PowerOpt:run()
	local sys_cmds = { "systemctl", "loginctl" } -- Commands to check
	for _, cmd in ipairs(sys_cmds) do
		if gears.filesystem.get_command_path(cmd) then
			awful.spawn(string.format("%s %s", cmd, self.action))
			return
		end
	end
	notify {
		title = "Error",
		text = "Couldn't find system commands: `systemctl`, `loginctl`"
	}
end

---@class PowerSection
---@field chosen number
local PowerSection = {}

--- Create a new PowerSection
function PowerSection:new()
	local section
	section = wibox.widget {
		buttons = awful.button(nil, 1, function() section:select() end),
		layout = wibox.layout.fixed.vertical,
		PowerOpt:new(icons.suspend, "suspend"),
		PowerOpt:new(icons.reboot, "reboot"),
		PowerOpt:new(icons.shutdown, "poweroff")
	}
	wibox.add_clickable(section)
	setmetatable(section, { __index = self })
	section.chosen = 1

	section:connect_signal(
		"mouse::enter",
		function()
			if awful.keygrabber.current_instance then return end
			execute_keybinding(nil, "Tab")
		end
	)
	for i, power_opt in ipairs(section:get_children()) do
		power_opt:connect_signal(
			"mouse::enter",
			function()
				section.chosen = i
				section:highlight_selected()
			end
		)
	end

	return section
end

function PowerSection:highlight_selected()
	for i, power_opt in ipairs(self:get_children()) do
		if i == self.chosen then
			power_opt:highlight()
		else
			power_opt:unhighlight()
		end
	end
end

--- Runs the associated action for the selected PowerOpt
function PowerSection:select()
	self:get_children()[self.chosen]:run()
end

--- Select the next PowerOpt
function PowerSection:select_next()
	local opts = self:get_children()
	if self.chosen == #opts then return end
	opts[self.chosen]:unhighlight()
	self.chosen = self.chosen + 1
	opts[self.chosen]:highlight()
end

--- Select the previous PowerOpt
function PowerSection:select_prev()
	local opts = self:get_children()
	if self.chosen == 1 then return end
	opts[self.chosen]:unhighlight()
	self.chosen = self.chosen - 1
	opts[self.chosen]:highlight()
end

--- Focus to the PowerSection (Enables a keygrabber)
function PowerSection:focus()
	local keygrabber
	local fn_next = function() self:select_next() end
	local fn_prev = function() self:select_prev() end
	local fn_select = function()
		keygrabber:stop()
		dashboard.toggle()
		self:select()
	end
	local fn_stop = function()
		keygrabber:stop()
		dashboard.toggle()
	end
	local fn_switch = function()
		keygrabber:stop()
		self:get_children()[self.chosen]:unhighlight()
		dashboard.launcher:focus()
	end

	keygrabber = awful.keygrabber {
		autostart = true,
		keybindings = {
			{ {},            "k",      fn_prev },
			{ {},            "j",      fn_next },
			{ {},            "Up",     fn_prev },
			{ {},            "Down",   fn_next },
			{ {},            "Return", fn_select },
			{ {},            "Escape", fn_stop },
			{ {},            "Tab",    fn_switch },
			{ { "Control" }, "p",      fn_prev },
			{ { "Control" }, "n",      fn_next },
			{ { "Control" }, "j",      fn_select },
			{ { "Control" }, "g",      fn_stop },
		}
	}

	self:get_children()[self.chosen]:highlight()
end

---@class Entry
---@field name string
---@field height number
---@field appinfo Gio.AppInfo
local Entry = {}
Entry.mt = {
	__lt = function(first_entry, second_entry)
		return first_entry.name < second_entry.name
	end,
	__le = function(first_entry, second_entry)
		return first_entry.name <= second_entry.name
	end,
	__index = Entry
}

--- Creates a new Entry object
---@param appinfo Gio.AppInfo
function Entry.new(appinfo)
	local name = appinfo:get_name()

	local textbox = wibox.widget {
		widget = wibox.widget.textbox,
		forced_width = beautiful.dashboard_text_width,
		text = " " .. name:gsub(
			".*",
			{
				["&"] = "&amp;",
				["<"] = "&lt;",
				["'"] = "&#39;",
			}
		)
	}
	local _, textbox_height = textbox:get_preferred_size(awful.screen.focused())
	textbox.forced_height = textbox_height

	local icon = appinfo:get_icon()
	local imagebox = nil
	if icon then
		imagebox = {
			widget = wibox.widget.imagebox,
			forced_height = textbox_height,
			forced_width = textbox_height,
			image = default_icon_theme:lookup_icon(
				icon:to_string(),
				64,
				0
			):get_filename()
		}
	end

	local entry = wibox.widget {
		widget = wibox.container.background,
		name = name,
		appinfo = appinfo,
		height = textbox_height,
		{
			widget = wibox.container.margin,
			margins = beautiful.dashboard_margins,
			{
				layout = wibox.layout.fixed.horizontal,
				imagebox,
				textbox
			}
		}

	}
	setmetatable(entry, Entry.mt)
	return entry
end

--- Launch an entry
function Entry:run()
	notify {
		title = "Launching Application",
		text = self.name
	}
	self.appinfo:launch()
end

--- Change the background color of the entry
function Entry:highlight()
	self:set_bg(beautiful.dashboard_chosen_bg)
end

--- Revert the background color of the entry
function Entry:unhighlight()
	self:set_bg(nil)
end

---@class LauncherSection
---@field entries table
---@field offset number
---@field chosen number
---@field textbox wibox.widget.textbox
---@field grid wibox.layout.grid
---@field scrollbar wibox.widget.imagebox
local LauncherSection = {}
local LauncherSection_mt = { __index = LauncherSection }

--- Create a new LauncherSection
function LauncherSection:new()
	local section
	local textbox = wibox.widget { widget = wibox.widget.textbox }
	local grid = wibox.widget {
		layout = wibox.layout.grid,
		homogeneous = false
	}
	local scrollbar = wibox.widget {
		widget = wibox.widget.imagebox,
		forced_width = beautiful.dashboard_scrollbar_width,
		resize = false
	}
	section = wibox.widget {
		layout = wibox.layout.fixed.vertical,
		spacing = beautiful.dashboard_launcher_spacing,
		buttons = gears.table.join(
			awful.button({}, 1, function()
				local entry = section.entries_filtered[section.chosen]
				if not entry then return end
				entry:run()
				dashboard.toggle()
			end),
			awful.button({}, 4, function() section:scrollUp() end),
			awful.button({}, 5, function() section:scrollDown() end)
		),
		{
			layout = wibox.layout.fixed.horizontal,
			spacing = beautiful.dashboard_launcher_spacing,
			{
				widget = wibox.widget.imagebox,
				image = icons.search,
				forced_height = beautiful.dashboard_search_icon_size,
				forced_width = beautiful.dashboard_search_icon_size
			},
			textbox
		},
		{
			layout = wibox.layout.fixed.horizontal,
			grid,
			scrollbar
		}
	}
	section.scrollbar = scrollbar
	section.textbox = textbox
	section.grid = grid
	section.offset = 0
	section.chosen = 1
	section.saved_text = ""
	setmetatable(section, LauncherSection_mt)

	local mouse_handler = function(entry)
		for i, e in ipairs(section.entries_filtered) do
			if e == entry then
				e:highlight()
				section.chosen = i
			else
				e:unhighlight()
			end
		end
	end

	--- Get/Create Entries
	section.entries = {}
	for _, appinfo in ipairs(Gio.AppInfo.get_all()) do
		if appinfo:should_show() then
			local entry = Entry.new(appinfo)

			entry:connect_signal("mouse::enter", mouse_handler)

			table.insert(section.entries, entry)
		end
	end
	table.sort(section.entries)

	section:focus()

	section:connect_signal(
		"mouse::enter",
		function()
			if awful.keygrabber.current_instance then
				execute_keybinding(nil, "Tab")
			end
		end
	)
	return section
end

--- Filter and redraw the grid and scrollbar of a LauncherSection
---@param search_string string|nil Won't filter if search_string is nil
function LauncherSection:filter_and_redisplay(search_string)
	local entry_amount
	if search_string then
		self.entries_filtered = {}
		entry_amount = 0
		for _, entry in ipairs(self.entries) do
			if entry.name:lower():match(search_string:lower()) then
				entry_amount = entry_amount + 1
				table.insert(self.entries_filtered, entry)
			end
		end
	else
		entry_amount = #self.entries_filtered
	end

	self.chosen = math.max(self.chosen, 1)
	self.chosen = math.min(self.chosen, entry_amount)

	if self.chosen > self.offset + beautiful.dashboard_limit then
		self.offset = self.offset + 1
	elseif self.chosen > 0 and self.chosen == self.offset then
		self.offset = self.offset - 1
	end

	if self.offset + beautiful.dashboard_limit > entry_amount then
		self.offset = math.max(entry_amount - beautiful.dashboard_limit, 0)
	end

	self.grid:reset()
	local index_start = self.offset + 1
	local index_end = math.min(
		entry_amount,
		self.offset + beautiful.dashboard_limit
	)
	for i = index_start, index_end do
		local entry = self.entries_filtered[i]
		-- Add entry to grid
		self.grid:add(entry)
		if i == self.chosen then
			entry:highlight()
		else
			entry:unhighlight()
		end
	end

	--- Scrollbar
	self.scrollbar.image = nil
	self.scrollbar.forced_height = nil
	local sample_entry = self.entries_filtered[1]
	if not sample_entry then return end
	local h = sample_entry.height + beautiful.dashboard_margins * 2
	h = h * math.min(entry_amount, beautiful.dashboard_limit)

	local r = entry_amount / h
	self.scrollbar.forced_height = h
	self.scrollbar.image = cairo.CreateImage(
		function(cr)
			cr:set_source(gears.color(beautiful.fg_normal))
			cr:rectangle(0, self.offset / r, 2, beautiful.dashboard_limit / r)
			cr:fill(((entry_amount - beautiful.dashboard_limit) / r))
		end,
		{ 2, h }
	)
end

--- Starts the Keygrabber for a LauncherSection
function LauncherSection:focus()
	local fn_next = function()
		self.chosen = self.chosen + 1
		if self.chosen > self.offset + beautiful.dashboard_limit then
			self.offset = self.offset + 1
		end
	end
	local fn_prev = function()
		self.chosen = self.chosen - 1
	end
	local fn_switch = function()
		local entry = self.entries_filtered[self.chosen]
		if entry then entry:unhighlight() end
		dashboard.power:focus()
	end

	awful.prompt.run {
		textbox = self.textbox,
		bg_cursor = beautiful.fg_normal,
		hooks = {
			{ {},            "Tab",    fn_switch },
			{ {},            "Escape", dashboard.toggle },
			{ { "Control" }, "g",      dashboard.toggle }
		},
		changed_callback = function(text)
			self:filter_and_redisplay(text)
			self.saved_text = text
		end,
		keypressed_callback = function(mod, key)
			local keys = {
				["n"] = { mod = "Control", callback = fn_next },
				["p"] = { mod = "Control", callback = fn_prev },
				["Down"] = { callback = fn_next },
				["Up"] = { callback = fn_prev }
			}

			local captured_key = keys[key]
			if not captured_key then return end
			if captured_key.mod == nil or mod[captured_key.mod] then
				captured_key.callback()
			end
		end,
		exe_callback = function(text)
			local entry = self.entries_filtered[self.chosen]
			if entry then
				entry:run()
				dashboard.toggle()
			elseif text then
				awful.spawn.with_shell(text)
				notify {
					title = "Running Command",
					text = text
				}
				dashboard.toggle()
			end
		end
	}

	self:filter_and_redisplay("")
end

--- Scroll the LauncherSection up
function LauncherSection:scrollUp()
	if self.offset < 1 then return end
	self.offset = self.offset - 1
	self.chosen = self.chosen - 1
	self:filter_and_redisplay()
end

--- Scroll the LauncherSection down
function LauncherSection:scrollDown()
	if self.offset == #self.entries_filtered - beautiful.dashboard_limit then return end
	self.offset = self.offset + 1
	self.chosen = self.chosen + 1
	self:filter_and_redisplay()
end

--- Toggle the dashboard
function dashboard.toggle()
	if dashboard.popup then
		execute_keybinding(nil, "Escape")
		dashboard.popup.visible = false
		dashboard.popup = nil
		return
	end
	dashboard.power = PowerSection:new()
	dashboard.launcher = LauncherSection:new()
	dashboard.popup = awful.popup {
		ontop = true,
		visible = true,
		widget = {
			widget = wibox.container.border,
			borders = { "right", "bottom" },
			{
				widget = wibox.container.margin,
				margins = beautiful.dashboard_margins,
				{
					layout = wibox.layout.fixed.horizontal,
					spacing = beautiful.dashboard_spacing,
					spacing_widget = {
						widget = wibox.widget.separator,
						color = beautiful.dashboard_separator_color
					},
					{
						widget = wibox.container.margin,
						margins = beautiful.dashboard_margins,
						dashboard.power
					},
					{
						widget = wibox.container.margin,
						margins = beautiful.dashboard_margins,
						dashboard.launcher
					}
				}
			}
		}
	}

	awful.placement.next_to(
		dashboard.popup,
		{
			preferred_positions = "bottom",
			preferred_anchors = "front",
			geometry = awful.screen.focused().wibar
		}
	)
	dashboard.popup.y = dashboard.popup.y - beautiful.border_width
	dashboard.popup.x = dashboard.popup.x - beautiful.border_width
end

-- GLib.idle_add(1, cache_icon)
Gdk.threads_add_idle(GLib.PRIORITY_DEFAULT_IDLE, cache_icon)
return dashboard
