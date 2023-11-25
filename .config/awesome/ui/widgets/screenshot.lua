local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local notify = require("naughty").notify
local dpi = beautiful.xresources.apply_dpi
local gears = require("gears")
local icon = require("ui.icons")
local utils = require("ui.utils")


local function style_for_pango(text, style)
	local options = ""
	for key, v in pairs(style) do
		options = options .. string.format(" %s='%s'", key, v)
	end
	return string.format("<span%s>%s</span>", options, text)
end

local templates = {}

templates.option = function(text, callback)
	return {
		widget = wibox.container.background,
		callback = callback,
		id = "option",
		{
			widget = wibox.container.margin,
			margins = dpi(10),
			{
				widget = wibox.widget.textbox,
				markup = style_for_pango(text, { font = 12 }),
				align = "center"
			}
		}
	}
end

templates.path = function()
	return os.date("'/tmp/Screenshot %d-%m-%Y %X.png'")
end

local function place(p)
	awful.placement.next_to(
		p,
		{
			preferred_positions = "bottom",
			preferred_anchors = "middle",
			geometry = awful.screen.focused().wibar
		}
	)
	p.y = p.y - beautiful.border_width
end

local popup = awful.popup {
	placement = place,
	ontop = true,
	visible = false,
	widget = utils.border_wrapper({
			widget = wibox.container.margin,
			margins = dpi(10),
			{
				layout = wibox.layout.fixed.vertical,
				spacing = dpi(10),
				{
					widget = wibox.widget.textbox,
					align = "center",
					markup = style_for_pango("Screenshot", {
							font = 12,
							fgcolor = beautiful.border_focus,
							weight = "bold"
					})
				},
				{
					layout = wibox.layout.grid,
					orientation = "horizontal",
					spacing = dpi(5),
					templates.option("Full", function()
							local path = templates.path()
							awful.spawn.easy_async_with_shell(
								"sleep 0.05; import -window root " .. path,
								function(_, _, _, exit_code)
									if exit_code ~= 0 then return end
									notify {
										title = "Took Screenshot",
										text = path
									}
								end
							)
					end),
					templates.option("Partial", function()
							local path = templates.path()
							awful.spawn.easy_async_with_shell(
								"sleep 0.05; import " .. path,
								function(_, _, _, exit_code)
									if exit_code ~= 0 then return end
									notify {
										title = "Took Screenshot",
										text = path
									}
								end
							)
					end)
				}
			}
	}, { top = true })
}

popup.options = popup.widget:get_children_by_id("option")

function popup.update()
	for i, w in ipairs(popup.options) do
		if i == popup.chosen then
			w.bg = "#00000030"
		else
			w.bg = nil
		end
	end
end

function popup.prev()
	if popup.chosen > 1 then
		popup.chosen = popup.chosen - 1
	end
	popup.update()
end

function popup.next()
	if popup.chosen < #popup.options then
		popup.chosen = popup.chosen + 1
	end
	popup.update()
end

function popup.press()
	popup.options[popup.chosen].callback()
	popup.toggle()
end

function popup.toggle()
	if not gears.filesystem.find_executable("import") then
		notify {
			title = "Screenshot",
			text = "Couldn't open screenshot tool, `import` isn't installed"
		}
		return
	end

	if popup.visible then
		popup.visible = false
		popup.keygrabber:stop()
		return
	end
	popup.chosen = 1
	popup.update()
	place(popup)
	popup.visible = true
	popup.keygrabber:start()
end

popup.widget:buttons(awful.button({}, 1, popup.press))

for i, w in ipairs(popup.options) do
	w:connect_signal("mouse::enter", function()
			popup.chosen = i
			popup.update()
	end)
end

popup.keygrabber = awful.keygrabber {
	keybindings = {
		{ {}, "Left", popup.prev },
		{ {}, "Right", popup.next },
		{ {}, "h", popup.prev },
		{ {}, "l", popup.next },
		{ {}, "Escape", popup.toggle },
		{ {}, "Return", popup.press },
		{ {"Control"}, "p", popup.prev },
		{ {"Control"}, "n", popup.next },
		{ {"Control"}, "g", popup.toggle },
		{ {"Control"}, "j", popup.press }
	}
}

local widget = {
	widget = wibox.container.background,
	shape = templates.rounded_rect,
	bg = "#00000030",
	buttons = awful.button({}, 1, popup.toggle),
	{
		widget = wibox.widget.imagebox,
		image = icon.screenshot
	}
}

awful.widget.screenshot = setmetatable(
	{ popup = popup.toggle },
	{ __call = function() return widget end }
)
