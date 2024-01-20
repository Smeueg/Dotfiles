local cursor = require("lib.cursor")
local wibox = require("wibox")
local notify = require("naughty").notify
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local icons = require("ui.icons")
local lgi = require("lgi")
local Gio = lgi.Gio
local dpi = beautiful.xresources.apply_dpi
local cairo = lgi.cairo
local utils = require("ui.utils")

local templates = {
	power_opt = function(icon, callback)
		local widget = wibox.widget {
			widget = wibox.container.background,
			id = "power_opt",
			callback = function()
				notify {
					title = "System",
					text = callback:capitalize()
				}
				if gears.filesystem.get_command_path("systemctl") then
					awful.spawn("systemctl " .. callback)
				else
					awful.spawn("loginctl " .. callback)
				end
			end,
			shape = gears.shape.rounded_rect_auto,
			{
				widget = wibox.container.margin,
				margins = dpi(10),
				{
					widget = wibox.widget.imagebox,
					image = icon,
					forced_height = dpi(25),
					forced_width = dpi(25)
				}
			}
		}
		cursor.add_clickable_to_wibox(widget)
		return widget
	end
}

local popup = awful.popup {
	ontop = true,
	visible = false,
	widget = utils.border_wrapper({
			widget = wibox.container.margin,
			margins = dpi(10),
			{
				layout = wibox.layout.fixed.horizontal,
				spacing_widget = {
					widget = wibox.widget.separator,
					color = "#ffffff10"
				},
				spacing = dpi(1),
				{
					widget = wibox.container.margin,
					margins = dpi(10),
					id = "power"
				},
				{
					widget = wibox.container.margin,
					margins = dpi(10),
					id = "launcher"
				}
			}
	}, { top = true, left = true })
}

popup.power = popup.widget:get_children_by_id("power")[1]
popup.launcher = popup.widget:get_children_by_id("launcher")[1]
popup.power.widget = wibox.widget {
	layout = wibox.layout.fixed.vertical,
	templates.power_opt(icons.suspend, "suspend"),
	templates.power_opt(icons.reboot, "reboot"),
	templates.power_opt(icons.shutdown, "poweroff"),
}

popup.power.options = popup.power.widget:get_children()

popup.launcher.widget = wibox.widget {
	layout = wibox.layout.fixed.vertical,
	spacing = dpi(5),
	{
		layout = wibox.layout.fixed.horizontal,
		spacing = dpi(5),
		{
			widget = wibox.widget.imagebox,
			image = icons.search,
			forced_height = dpi(25),
			forced_width = dpi(25)
		},
		{
			widget = wibox.widget.textbox,
			id = "textbox"
		}
	},
	{
		layout = wibox.layout.fixed.horizontal,
		{
			layout = wibox.layout.grid,
			id = "grid",
			homogeneous = false
		},
		{
			widget = wibox.widget.imagebox,
			id = "scrollbar",
			forced_width = dpi(10),
			resize = false
		}
	}
}
do
	local launcher = popup.launcher
	launcher.grid = launcher.widget:get_children_by_id("grid")[1]
	launcher.textbox = launcher.widget:get_children_by_id("textbox")[1]
	launcher.scrollbar = launcher.widget:get_children_by_id("scrollbar")[1]
end

function execute_keybinding(modifiers, key)
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

function popup.find_current_entry_pos(entry)
	local launcher = popup.launcher
	for i, e in ipairs(launcher.entries_filtered) do
		if e == entry then
			return i
		end
	end
end

function popup.get_entries() -- Load Entries as widgets to the grid
	local launcher = popup.launcher
	launcher.entries = {}
	for _, e in ipairs(Gio.AppInfo.get_all()) do
		if e:should_show() then
			local launch_fn = function() e:launch() end
			local name = e:get_name():gsub(".*", {
					["&"] = "&amp;",
					["<"] = "&lt;",
					["'"] = "&#39;",
			})

			local w = wibox.widget {
				widget = wibox.container.background,
				launch = launch_fn,
				{
					widget = wibox.container.margin,
					margins = beautiful.dashboard_margins,
					{
						widget = wibox.widget.textbox,
						text = name,
						forced_width = beautiful.dashboard_text_width
					}
				}

			}

			w:connect_signal("mouse::enter", function()
					launcher.chosen = popup.find_current_entry_pos(w)
					launcher.filter(launcher.text)
			end)

			cursor.add_clickable_to_wibox(w)

			table.insert(popup.launcher.entries, w)
		end
	end
end

function popup.launcher.filter(text)
	local launcher = popup.launcher
	local offset = launcher.offset
	local limit = launcher.limit
	launcher.text = launcher.textbox.text:gsub(" +$", "") -- remove trailing ws
	launcher.grid:reset()

	launcher.entries_filtered = {}
	for _, w in ipairs(launcher.entries) do
		if w.widget.widget.text:lower():match(text:lower()) then
			table.insert(launcher.entries_filtered, w)
		end
	end

	if launcher.chosen > #launcher.entries_filtered then
		launcher.chosen = #launcher.entries_filtered
	end

	if launcher.chosen < 1 then
		launcher.chosen = 1
	end

	for i = offset + 1, offset + limit do
		local w = launcher.entries_filtered[i]
		if w then
			launcher.grid:add(w)
			if i == launcher.chosen then
				w.bg = "#00000030"
			else
				w.bg = nil
			end
		end
	end

	-- Create the Scrollbar
	launcher.scrollbar.image = nil
	launcher.scrollbar.forced_height = nil
	local sample = launcher.entries_filtered[1]
	if sample then
		sample = sample.widget.widget
		local _, h = sample:get_preferred_size(awful.screen.focused())
		h = h + beautiful.dashboard_margins * 2
		if #launcher.entries_filtered < limit then
			h = h * #launcher.entries_filtered
		else
			h = h * limit
		end

		local r = #launcher.entries_filtered / h
		launcher.scrollbar.forced_height = h
		launcher.scrollbar.image = cairo.CreateImage(function(cr)
				cr:set_source(gears.color(beautiful.fg_normal))
				cr:rectangle(0, offset / r, 2, limit / r)
				cr:fill(((#launcher.entries_filtered - limit) / r))
		end, { 2, h })
	end
end

function popup.launcher.next()
	local launcher = popup.launcher
	if launcher.chosen < #launcher.entries_filtered then
		launcher.chosen = launcher.chosen + 1
	end

	if launcher.chosen > launcher.offset + launcher.limit then
		launcher.offset = launcher.offset + 1
	end
end

function popup.launcher.prev()
	local launcher = popup.launcher
	if launcher.chosen > 1 then
		launcher.chosen = launcher.chosen - 1
	end

	if launcher.chosen == launcher.offset then
		launcher.offset = launcher.offset - 1
	end
end

function popup.launcher.press()
	local launcher = popup.launcher
	local entry = launcher.entries_filtered[launcher.chosen]
	if entry then
		entry.launch()
		notify {
			title = "Launching Application",
			text = entry.widget.widget.text
		}
	else
		local cmd = launcher.text
		awful.spawn.with_shell(cmd)
		notify {
			title = "Running Command",
			text = cmd
		}
	end
	popup.visible = false
end

function popup.launcher.scroll(amount)
	local launcher = popup.launcher
	launcher.offset = launcher.offset + amount
	launcher.chosen = launcher.chosen + amount
	if launcher.offset + launcher.limit > #launcher.entries_filtered then
		launcher.offset = launcher.offset - 1
	end

	if launcher.chosen < launcher.offset + 1 then
		launcher.chosen = launcher.chosen + 1
	end

	launcher.filter(launcher.text)
end

function popup.launcher.scroll_up()
	local launcher = popup.launcher
	if launcher.offset > 0 then
		launcher.chosen = launcher.chosen - 1
		launcher.offset = launcher.offset - 1
	end
	launcher.filter(launcher.text)
end

function popup.launcher.scroll_down()
	local launcher = popup.launcher
	if launcher.offset + launcher.limit < #launcher.entries_filtered then
		launcher.chosen = launcher.chosen + 1
		launcher.offset = launcher.offset + 1
	end
	launcher.filter(launcher.text)
end

function popup.launcher.start()
	local launcher = popup.launcher
	launcher.chosen = 1
	launcher.offset = 0
	launcher.limit = beautiful.dashboard_limit
	awful.prompt.run {
		textbox = popup.launcher.textbox,
		bg_cursor = beautiful.fg_normal,
		changed_callback = popup.launcher.filter,
		hooks = {
			{ {}, "Tab", function()
					launcher.textbox.text = launcher.text .. " "
					for _, w in ipairs(launcher.entries_filtered) do
						w.bg = nil
					end
					popup.power.start()
			end },
			{ {},            "Escape", function() popup.visible = false end },
			{ { "Control" }, "g",      function() popup.visible = false end },
		},
		keypressed_callback = function(mod, key)
			local keys = {
				{ n = launcher.next,   mod = "Control" },
				{ p = launcher.prev,   mod = "Control" },
				{ Down = launcher.next },
				{ Up = launcher.prev },
			}
			for _, k in ipairs(keys) do
				if k[key] and (k.mod or true or mod[k.mod]) then
					k[key]()
				end
			end
		end,
		exe_callback = launcher.press
	}
	launcher.filter("")
end

popup.launcher.grid:buttons(gears.table.join(
		awful.button({}, 4, popup.launcher.scroll_up),
		awful.button({}, 5, popup.launcher.scroll_down)
))

-- popup.power
function popup.power.update()
	local power = popup.power
	for i, w in ipairs(power.options) do
		if i == power.chosen then
			w.bg = "#00000030"
		else
			w.bg = nil
		end
	end
end

function popup.power.stop()
	local power = popup.power
	power.keygrabber:stop()
	power.chosen = 0
	power.update()
end

function popup.power.exit()
	popup.power.stop()
	popup.visible = false
end

function popup.power.switch()
	popup.power.stop()
	popup.launcher.start()
end

function popup.power.next()
	local power = popup.power
	if power.chosen < #power.options then
		power.chosen = power.chosen + 1
		power.update()
	end
end

function popup.power.prev()
	local power = popup.power
	if power.chosen > 1 then
		power.chosen = power.chosen - 1
		power.update()
	end
end

function popup.power.press()
	local power = popup.power
	power.options[power.chosen].callback()
	power.exit()
end

function popup.power.start()
	local power = popup.power
	power.chosen = 1
	popup.power.update()

	power.keygrabber:start()
end

function popup.toggle()
	if popup.visible then
		execute_keybinding(nil, "Escape")
		popup.visible = false
		return
	end


	local launcher = popup.launcher
	local power = popup.power
	popup.visible = true
	awful.placement.next_to(
		popup,
		{
			preferred_positions = "bottom",
			preferred_anchors = "front",
			geometry = awful.screen.focused().wibar
		}
	)
	popup.y = popup.y - beautiful.border_width
	popup.x = popup.x - beautiful.border_width
	popup.get_entries()

	-- Default Values
	local themes_default = {
		dashboard_margins = dpi(10),
		dashboard_text_width = dpi(300),
		dashboard_limit = 10
	}
	for k, v in pairs(themes_default) do
		beautiful[k] = beautiful[k] or v
	end
	launcher.text = nil
	launcher.chosen = 1
	launcher.offset = 0
	launcher.start()
	power.chosen = 1
	power.keygrabber = awful.keygrabber {
		keybindings = {
			{ {},          "j",      power.next },
			{ {},          "k",      power.prev },
			{ {},          "Down",   power.next },
			{ {},          "Up",     power.prev },
			{ {},          "Escape", power.exit },
			{ {},          "Return", power.press },
			{ {},          "Tab",    power.switch },
			{ { "Control" }, "n",    power.next },
			{ { "Control" }, "p",    power.prev },
			{ { "Control" }, "g",    power.exit },
			{ { "Control" }, "j",    power.press }
		}
	}
end

-- Mouse support for popup.power
for i, w in ipairs(popup.power.options) do
	w:connect_signal("mouse::enter", function()
			popup.power.chosen = i
			popup.power.update()
	end)
end

popup.power.widget:buttons(awful.button({}, 1, popup.power.press))
popup.power:connect_signal("mouse::enter", function()
		if awful.keygrabber.current_instance ~= popup.power.keygrabber then
			execute_keybinding(nil, "Tab")
		end
end)

-- Mouse support for popup.launcher
popup.launcher:connect_signal("mouse::enter", function()
		if awful.keygrabber.current_instance == popup.power.keygrabber then
			execute_keybinding(nil, "Tab")
		end
end)



local widget = wibox.widget {
	widget = wibox.container.background,
	shape = gears.shape.rounded_rect_auto,
	bg = "#00000030",
	buttons = gears.table.join(
		awful.button({}, 1, popup.toggle)
	),
	{
		widget = wibox.container.margin,
		margins = dpi(6),
		{
			widget = wibox.widget.imagebox,
			image = icons.dashboard
		}
	}
}

cursor.add_clickable_to_wibox(widget)


awful.widget.dashboard = setmetatable(
	{ popup = popup.toggle },
	{ __call = function() return widget end }
)
