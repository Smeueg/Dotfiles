--------------------------------------------------------------------------------
--- A screenshot popup
---
--- @author Smeueg (https://github.com/Smeueg)
--- @copyright 2024 Smeueg
--------------------------------------------------------------------------------
local awful = require("awful")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local wibox = require("wibox")
local notify = require("naughty").notify
local module = {}


--- Takes a screenshot of the system
--- (see: import(1))
--- @param screenshot_type "full"|"partial"
local function screenshot(screenshot_type)
	local import_flags
	local filepath = os.date("'/tmp/Screenshot %d-%m-%Y %X.png'")
	if screenshot_type == "full" then
		import_flags = "-window root"
	elseif screenshot_type == "partial" then
		import_flags = ""
	else
		error("screenshot() expects a \"full\" or \"partial\"as an argument")
	end

	awful.spawn.easy_async(
		string.format("import -silent %s %s", import_flags, filepath),
		function(_, _, _, exit_code)
			if exit_code ~= 0 then
				notify { text = "Failed To Take Screenshot" }
			else
				notify {
					title = "Took Screenshot",
					text = filepath
				}
			end
		end
	)
end


--- Applies a template for an option for the popup widget
---@param text string
---@param screenshot_type "full"|"partial"
local function option(text, screenshot_type)
	local widget = wibox.widget {
		widget = wibox.container.background,
		screenshot_type = screenshot_type,
		id = "option",
		{
			widget = wibox.container.margin,
			margins = dpi(10),
			{
				widget = wibox.widget.textbox,
				markup = text:to_pango { font = 12 },
				cursor = "hand1",
				align = "center"
			}
		}
	}
	wibox.add_clickable(widget)
	return widget
end



---@class Popup
---@field widget wibox.widget
---@field options table The table of options
---@field options_len number The amount of options
---@field option_index number The index of the currently chosen option
---@field keygrabber awful.keygrabber The popup's keygrabber
local ScreenshotPopup = setmetatable(
	{},
	{
		--- Creates a new popup
		__call = function(self)
			local screenshot_popup = setmetatable({}, { __index = self })

			screenshot_popup.option_index = 1
			screenshot_popup.options = {
				option("Full", "full"),
				option("Partial", "partial")
			}

			screenshot_popup.popup = awful.popup {
				placement = self.place,
				ontop = true,
				visible = true,
				widget = {
					widget = wibox.container.border,
					borders = { "left", "right", "bottom" },
					{
						widget = wibox.container.margin,
						id = "widget",
						buttons = awful.button(
							nil, 1, nil,
							function() screenshot_popup:press() end
						),
						margins = dpi(10),
						{
							layout = wibox.layout.fixed.vertical,
							spacing = dpi(10),
							{
								widget = wibox.widget.textbox,
								align = "center",
								markup = ("Screenshot"):to_pango {
									font = 12,
									fgcolor = beautiful.border_focus,
									weight = "bold"
								}
							},
							{
								layout = wibox.layout.grid,
								orientation = "horizontal",
								spacing = dpi(5),
								table.unpack(screenshot_popup.options)
							}
						}
					}
				}
			}

			local fn_press = function() screenshot_popup:press() end
			local fn_prev = function()
				screenshot_popup:change_chosen(screenshot_popup.option_index - 1)
			end
			local fn_next = function()
				screenshot_popup:change_chosen(screenshot_popup.option_index + 1)
			end

			screenshot_popup.keygrabber = awful.keygrabber {
				keybindings = {
					{ {},            "Left",   fn_prev },
					{ {},            "Right",  fn_next },
					{ {},            "h",      fn_prev },
					{ {},            "l",      fn_next },
					{ {},            "Escape", module.toggle },
					{ {},            "Return", fn_press },
					{ { "Control" }, "p",      fn_prev },
					{ { "Control" }, "n",      fn_next },
					{ { "Control" }, "g",      module.toggle },
					{ { "Control" }, "j",      fn_press }
				}
			}

			screenshot_popup.keygrabber:start()

			screenshot_popup:change_chosen(1)

			for i, option in ipairs(screenshot_popup.options) do
				option:connect_signal("mouse::enter", function()
						screenshot_popup:change_chosen(i)
				end)
			end

			return screenshot_popup
		end
	}
)


--- Positions the popup to the bottom center of the wibar
function ScreenshotPopup:place()
	self.visible = false
	awful.placement.next_to(
		self,
		{
			preferred_positions = "bottom",
			preferred_anchors = "middle",
			geometry = awful.screen.focused().wibar
		}
	)
	self.y = self.y - beautiful.border_width
	self.visible = true
end

--- Destroys the popup while also turning of the keygrabber
function ScreenshotPopup:stop()
	self.popup.visible = false
	self.keygrabber:stop()
end

--- Changes the currently chosen option to the option with the new index
---@param new_index number
function ScreenshotPopup:change_chosen(new_index)
	if self.options[new_index] == nil then return end
	self.options[self.option_index].bg = nil
	self.option_index = new_index
	self.options[self.option_index].bg = beautiful.popup_chosen_bg
end

--- Takes a screenshot when enter is pressed or was clicked by the mouse
function ScreenshotPopup:press()
	screenshot(self.options[self.option_index].screenshot_type)
	module.toggle()
end

--- Toggles a global screenshot popup
function module.toggle()
	if module.popup then
		module.popup:stop()
		module.popup = nil
	else
		module.popup = ScreenshotPopup()
	end
end

return module
