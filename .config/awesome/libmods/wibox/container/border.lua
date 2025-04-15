--------------------------------------------------------------------------------
--- A container to add individual borders around a widget
--- 
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--- 
--- Relevant Documentation:
--- * https://awesomewm.org/doc/api/documentation/04-new-widgets.md.html
--- * https://awesomewm.org/doc/api/classes/wibox.widget.base.html
--------------------------------------------------------------------------------
local base      = require("wibox.widget.base")
local bget      = require("beautiful").get
local gcolor    = require("gears.color")
local gtable    = require("gears.table")
local setmetatable = setmetatable
local unpack = unpack or table.unpack

local border = { mt = {} }

--- Draw the borders
---@param _ any
---@param cr cairo.Context
---@param width number
---@param height number
function border:draw(_, cr, width, height)
	if not self._private.widget or not self._private.widget:get_visible() then
		return
	end

	local border_width = self._private.border_width
	cr:set_source(self._private.color)

	local border_map = {
		["top"]    = { 0, 0, width, border_width },
		["bottom"] = { 0, height - border_width, width, border_width },
		["left"]   = { 0, 0, border_width, height },
		["right"]  = { width - border_width, 0, border_width, height },
	}

	for _, b in ipairs(self._private.borders) do
		cr:rectangle(unpack(border_map[b]))
	end
	cr:fill()
end

--- The container's layout
---@param _ context
---@param width number
---@param height number
---@return table
function border:layout(_, width, height)
	if self._private.widget then
		return { base.place_widget_at(self._private.widget, 0, 0, width, height) }
	end
end

--- Fit the widget
---@param context context
---@param width number
---@param height number
---@return number
---@return number
function border:fit(context, width, height)
	if not self._private.widget then
		return 0, 0
	end

	return base.fit_widget(self, context, self._private.widget, width, height)
end

--- Set the color of the borders
---@param color string
function border:set_color(color)
	self._private.color = gcolor(color or bget().border_focus)
	self:emit_signal("widget::redraw_needed")
end

--- Set the border width of the borders
---@param border_width number
function border:set_border_width(border_width)
	self._private.border_width = border_width or bget().border_width
	self:emit_signal("widget::redraw_needed")
end

--- Set the of the borders
---@param borders ("top"|"bottom"|"left"|"right")[]
function border:set_borders(borders)
	self._private.borders = borders or {}
	self:emit_signal("widget::redraw_needed")
end

--- Set the widget to wrap with borders
---@param widget wibox.widget
function border:set_widget(widget)
	if widget then
		base.check_widget(widget)
	end
	self._private.widget = widget
	self:emit_signal("widget::layout_changed")
end

--- Get the widget that's being displayed
---@return wibox.widget
function border:get_widget()
	return self._private.widget
end

--- Set the children to wrap with borders
---@param children wibox.widget
function border:set_children(children)
	self:set_widget(children[1])
end

--- Get the widget that's being displayed
---@return wibox.widget
function border:get_children()
	return { self._private.widget }
end

--- Create a new border container
---@param widget wibox.widget
---@param borders ("top"|"bottom"|"left"|"right")[]
---@param color string
---@return wibox.widget
function border.mt:__call(widget, borders, borders_width, color)
	local container = base.make_widget(nil, nil, { enable_properties = true })

	gtable.crush(container, border, true)

	container:set_widget(widget)
	container:set_borders(borders)
	container:set_border_width(borders_width)
	container:set_color(color)

	return container
end

return setmetatable(border, border.mt)
