local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")
local layouts = awful.layout.suit
local module = {}

-- Set layout for all tags
function module.set_all(layout)
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

-- Increase/decrease the master width factor for all tags
function module.incmwfact(width)
	if awful.layout.get() ~= layouts.tile.right then return end
	for _, t in ipairs(root.tags()) do awful.tag.incmwfact(width, t) end
end

-- Icons
local icon_max, icon_tile, icon_floating
do
	local cairo = require("lgi").cairo
	local cairo_format = cairo.Format.ARGB32
	local cairo_create = cairo.ImageSurface.create
	local cairo_context = cairo.Context
	local transform = gears.shape.transform
	local color = gears.color(beautiful.wibar_selected_tag)
	local pi = math.pi
	local cr

	icon_max = cairo_create(cairo_format, 20, 20)
	cr = cairo_context(icon_max)
	cr:set_source(color)
	transform(gears.shape.rectangle):translate(4, 4)(cr, 12, 12)
	cr:fill()

	icon_tile = cairo_create(cairo_format, 20, 20)
	cr = cairo_context(icon_tile)
	cr:set_source(color)
	transform(gears.shape.rectangle):translate(4, 4)(cr, 5, 12)
	transform(gears.shape.rectangle):translate(11, 4)(cr, 5, 5)
	transform(gears.shape.rectangle):translate(11, 11)(cr, 5, 5)
	cr:fill()

	icon_floating = cairo_create(cairo_format, 20, 20)
	cr = cairo_context(icon_floating)
	cr:set_source(color)
	transform(gears.shape.rectangle):translate(4, 4)(cr, 8, 8)
	transform(gears.shape.rectangle):translate(8, 14)(cr, 8, 2)
	transform(gears.shape.rectangle):translate(14, 8)(cr, 2, 8)
	cr:fill()
end

local function toggle_popup()
	if module.popup then
		-- module.keygrabber:stop()
		module.popup.visible = false
		module.popup = nil
		return
	end

	local options = {
		layout = wibox.layout.grid,
		forced_num_cols = 3,
		{ icon = icon_tile, layout = layouts.tile.right},
		{ icon = icon_max, layout = layouts.max},
		{ icon = icon_floating, layout = layouts.floating},
	}

	for i, opt in ipairs(options) do
		options[i] = {
			widget = wibox.container.background,
			shape = gears.shape.rounded_rect,
			id = "option",
			func = function() module.set_all(opt.layout) end,
			{
				widget = wibox.container.margin,
				margins = 5,
				{
					widget = wibox.widget.imagebox,
					image = opt.icon,
					forced_height = 50,
					forced_width = 50
				}
			}
		}
	end

	module.popup = awful.popup {
		border_width = beautiful.border_width,
		border_color = beautiful.border_focus,
		placement = awful.placement.centered,
		shape = gears.shape.rounded_rect,
		ontop = true,
		widget = {
			widget = wibox.container.margin,
			margins = 20,
			{
				widget = wibox.container.background,
				shape = gears.shape.rounded_rect,
				options
			}
		}
	}

	local function update(widgets, chosen)
		for i, w in ipairs(widgets) do
			if i == chosen then
				w.bg = "#00000030"
			else
				w.bg = nil
			end
		end
	end

	local widgets = module.popup.widget:get_children_by_id("option")
	for i, w in ipairs(widgets) do
		w:connect_signal(
			"mouse::enter",
			function()
				for j, w in ipairs(widgets) do
					if j == i then
						w.bg = "#00000030"
					else
						w.bg = nil
					end
				end
			end
		)
		w:connect_signal(
			"button::press",
			function()
				widgets[i].func()
				toggle_popup()
			end
		)
	end
end


do -- Wibar widget
	module.widget = wibox.widget {
		widget = wibox.container.background,
		bg = "#00000030",
		shape = gears.shape.rounded_rect,
		buttons = awful.button({}, 1, toggle_popup),
		{
			widget = wibox.widget.imagebox,
			id = "icon",
			image = icon_tile
		}
	}

	local function update(t)
		if t == nil or t.layout == layouts.tile.right then
			icon = icon_tile
		elseif t.layout == layouts.max then
			icon = icon_max
		elseif t.layout == layouts.floating then
			icon = icon_floating
		end
		module.widget:get_children_by_id("icon")[1].image = icon
	end
	tag.connect_signal("property::layout", update)
	update()
end

return module
