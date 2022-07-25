local beautiful = require("beautiful")
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local cairo = require("lgi").cairo
local gcolor = gears.color
local cairo_format = cairo.Format.ARGB32
local cairo_create = cairo.ImageSurface.create
local cairo_context = cairo.Context
local transform = gears.shape.transform
local animate = require("animate")
local notify = require("naughty").notify
local pi = math.pi

local bar = {}

function bar.date()
	return {
		widget = wibox.container.background,
		bg = "#00000030",
		shape = gears.shape.rounded_rect,
		{
			widget = wibox.container.margin,
			left = 5,
			right = 5,
			{
				widget = wibox.widget.textclock,
				format = " %a - %d/%m/%y  %H:%M "
			}
		}
	}
end

function bar.network()
	local widget = wibox.widget {
		widget = wibox.container.background,
		bg = beautiful.tasklist_bg_focus,
		shape = gears.shape.rounded_rect,
		{
			layout = wibox.layout.fixed.horizontal,
			{
				widget = wibox.container.margin,
				id = "icon",
				{ widget = wibox.widget.imagebox }
			},
			{
				widget = wibox.widget.textbox,
				id = "text"
			}
		}
	}

	-- Icons
	local icon_wifi = cairo_create(cairo_format, 20, 20)
	local icon_ethernet = cairo_create(cairo_format, 20, 20)
	local icon_offline = cairo_create(cairo_format, 20, 20)
	local cr
	-- Wifi
	cr = cairo_context(icon_wifi)
	cr:set_source(gears.color(beautiful.menubar_bg_focus))
	gears.shape.transform(gears.shape.pie)
		:scale(1.25, 1.5)
		:translate(-1.5, 1.5)(cr, 20, 20, 1.25 * pi, 1.75 * pi)
	cr:fill()
	-- Ethernet
	cr = cairo.Context(icon_ethernet)
	cr:set_source(gears.color(beautiful.menubar_bg_focus))
	cr:rectangle(9, 2, 6, 6)
	cr:rectangle(4, 14, 6, 6)
	cr:rectangle(14, 14, 6, 6)
	cr:rectangle(11, 7, 2, 4)
	cr:rectangle(7, 10, 10, 2)
	cr:rectangle(6, 10, 2, 4)
	cr:rectangle(16, 10, 2, 4)
	cr:fill()
	-- Offline
	cr = cairo.Context(icon_offline)
	cr:set_source(gears.color(beautiful.menubar_bg_focus))
	gears.shape.transform(gears.shape.pie)
		:scale(1.15, 1.4)
		:translate(-1.5, 1.8)(cr, 20, 20, 1.25 * pi, 1.75 * pi)
	cr:stroke()

	local parse = function(stdout)
		local widget_icon, widget_text = nil, ""
		for net_name, net_type in stdout:gmatch("([^:]+):([^\n]+)") do
			widget_text	= widget_text .. " " .. net_name
			if net_type:match("[^-]+$") == "wireless" then
				widget_icon = icon_wifi
			else
				widget_icon = icon_ethernet
			end
		end

		widget_text = widget_text:match("^ (.*)")
		if widget_text == "" or widget_text == nil then
			widget_text = "Offline"
			widget_icon = icon_offline
		end

		widget:get_children_by_id("icon")[1].margins = 5
		widget:get_children_by_id("icon")[1].widget.image = widget_icon
		widget:get_children_by_id("text")[1].text = widget_text .. " "
	end

	widget.timer = gears.timer {
		timeout = 10,
		autostart = true,
		call_now = true,
		callback = function()
			awful.spawn.easy_async(
				"nmcli -t -f name,type connection show --active",
				parse
			)
		end
	}

	return widget
end

function bar.layout()
	local color = beautiful.menubar_bg_focus

	local cr
	local icon_max = cairo_create(cairo_format, 20, 20)
	cr = cairo.Context(icon_max)
	cr:set_source(gears.color(color))
	transform(gears.shape.rectangle):translate(4.5, 4.5)(cr, 11, 11)
	cr:fill()

	local icon_tile = cairo_create(cairo_format, 20, 20)
	cr = cairo_context(icon_tile)
	cr:set_source(gears.color(color))
	transform(gears.shape.rectangle):translate(4.5, 4.5)(cr, 5, 11)
	transform(gears.shape.rectangle):translate(10.5, 4.5)(cr, 5, 5)
	transform(gears.shape.rectangle):translate(10.5, 10.5)(cr, 5, 5)
	cr:fill()
	local icon_floating = cairo_create(cairo_format, 20, 20)
	cr = cairo_context(icon_floating)
	cr:set_source(gears.color(color))
	transform(gears.shape.rectangle):translate(4.5, 4.5)(cr, 8, 8)
	transform(gears.shape.rectangle):translate(13.5, 7.5)(cr, 2, 8)
	transform(gears.shape.rectangle):translate(7.5, 13.5)(cr, 8, 2)
	cr:fill()

	local widget = wibox.widget {
		widget = wibox.container.background,
		bg = "#00000030",
		shape = gears.shape.rounded_rect,
		{
			widget = wibox.widget.imagebox,
			id = "icon"
		}
	}

	local function update(t)
		local layouts = awful.layout.suit
		if t == nil or t.layout == layouts.tile.right then
			icon = icon_tile
		elseif t.layout == layouts.max then
			icon = icon_max
		elseif t.layout == layouts.floating then
			icon = icon_floating
		end

		widget:get_children_by_id("icon")[1].image = icon
	end

	tag.connect_signal("property::layout", update)
	update()

	return widget
end

function bar.tasklist_create(s)
	local buttons = gears.table.join(
		awful.button({}, 1, function(c)
				if c == client.focus then
					c.minimized = true
				else
					c:emit_signal("request::activate", "tasklist", { raise = true })
				end
		end),
		awful.button({}, 4, function() awful.client.focus.byidx(-1) end),
		awful.button({}, 5, function() awful.client.focus.byidx(1) end)
	)
	local update = function(self, c)
		local widget = self:get_children_by_id("bg")[1]
		local opacity = c == client.focus and 30 or 0
		animate.simple(
			tostring(widget), 0.1, widget.bg_opacity or 0, opacity,
			function(value)
				local h	= string.format("#000000%02i", math.ceil(value))
				widget.bg = h
				widget.bg_opacity = math.ceil(value)
			end
		)
	end
	return awful.widget.tasklist { -- Tasklist Widget
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = buttons,
		layout = {
			layout = wibox.layout.fixed.horizontal,
			spacing = 10
		},
		widget_template = {
			widget = wibox.container.background,
			shape = gears.shape.rounded_rect,
			create_callback = update,
			update_callback = update,
			id = "bg",
			{
				widget = wibox.container.margin,
				margins = 4,
				{
					id = "icon_role",
					widget = wibox.widget.imagebox
				}
			},
		}
	}
end

function bar.taglist_create(s)
	local buttons = gears.table.join(
		awful.button({}, 1, function(t) t:view_only() end),
		awful.button({}, 3, function(t)
				local c = client.focus
				if c then c:move_to_tag(t) end
		end),
		awful.button({}, 4, function(t) awful.tag.viewprev(t.screen) end),
		awful.button({}, 5, function(t) awful.tag.viewnext(t.screen) end)
	)
	local update = function(self, t)
		local icon = self:get_children_by_id("icon")[1]
		icon.bg = icon.bg or beautiful.bg_normal
		local icon_str = tostring(icon)
		icon.shape_border_color = icon.shape_border_color or
			beautiful.wibar_unselected_tag

		local border = t.selected and
			beautiful.wibar_selected_tag or
			beautiful.wibar_unselected_tag
		local bg = next(t:clients()) and border or beautiful.wibar_bg

		local _, r, g, b = icon.bg:get_rgba()
		local hex = string.format(
			"#%02X%02X%02X",
			math.floor(r * 255),
			math.floor(g * 255),
			math.floor(b * 255)
		)

		animate.color(
			icon_str .. "border", 0.1, icon.shape_border_color, border,
			function(color) icon.shape_border_color = color end
		)
		animate.color(
			icon_str .. "bg", 0.1, hex, bg,
			function(color)
				icon.bg = color
			end
		)
	end
	return awful.widget.taglist {
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = buttons,
		layout = { spacing = 2, layout = wibox.layout.fixed.horizontal },
		widget_template = {
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
			create_callback = update,
			update_callback = update,
		}
	}
end

for i, f in pairs(bar) do
	if debug.getinfo(f).nparams == 0 then
		bar[i] = f()
	end
end

return bar
