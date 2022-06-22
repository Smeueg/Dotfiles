local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local gcolor = gears.color
local cairo_format = cairo.Format.ARGB32
local cairo_create = cairo.ImageSurface.create
local cairo_context = cairo.Context
local transform = gears.shape.transform
local layouts = awful.layout.suit
local module = {}

local function set_layout_all(layout)
	-- Set layout for all tags
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

-- Icons
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

local options = {
	{
		icon = icon_tile,
		layout = layouts.tile.right
	},
	{
		icon = icon_max,
		layout = layouts.max
	},
	{
		icon = icon_floating,
		layout = layouts.floating
	},
}

for i, opt in ipairs(options) do
	options[i] = {
		widget = wibox.container.background,
		id = "option",
		func = function() set_layout_all(opt.layout) end,
		{
			widget = wibox.container.margin,
			margins = 5,
			{
				widget = wibox.widget.imagebox,
				image = opt.icon
			}
		}
	}
end

local function options_update(widgets)
	for i, w in ipairs(widgets) do
		if i == module.chosen then
			w.bg = "#00000030"
		else
			w.bg = nil
		end
	end
end

local function options_next(widgets)
	if module.chosen ~= #widgets then module.chosen = module.chosen + 1 end
	option_update(widgets)
end

local function options_prev(widgets)
	if module.chosen ~= 1 then module.chosen = module.chosen - 1 end
	option_update(widgets)
end

local function options_press(widgets)
	widgets[module.chosen].func()
	module.toggle()
end

function module.toggle()
	if module.popup then
		module.keygrabber:stop()
		module.popup.visible = false
		module.popup = nil
		return
	end

	module.chosen = 1
	module.popup = awful.popup {
		placement = awful.placement.centered,
		border_width = border_width,
		border_color = border_color,
		shape = gears.shape.rounded_rect,
		ontop = true,
		widget = {
			widget = wibox.container.margin,
			margins = 20,
			{
				widget = wibox.container.grid,
				forced_num_cols = #options,
			}
		}
	}

	local widgets = module.popup.widget:get_children_by_id("option")
	for i, w in ipairs(widgets) do
		w:connect_signal(
			"mouse::enter",
			function()
				module.chosen = i
				options_update(widgets)
			end
		)
		w:connect_signal(
			"button::press",
			function()
				options_press(widgets)
			end
		)
	end
	options_update(widgets)
end

module.widget = wibox.widget {
	widget = wibox.container.background,
	bg = "#00000030",
	shape = gears.shape.rounded_rect,
	{
		widget = wibox.widget.imagebox,
		image = icon_tile,
		id = "icon"
	}
}

local function update(t)
	local layouts = awful.layout.suit
	if t.layout == layouts.tile.right then
		icon = icon_tile
	elseif t.layout == layouts.max then
		icon = icon_max
	elseif t.layout == layouts.floating then
		icon = icon_floating
	end
	module.widget:get_children_by_id("icon")[1].image = icon
end

tag.connect_signal("property::layout", update)
