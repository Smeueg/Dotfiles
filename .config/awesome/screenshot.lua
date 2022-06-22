local awful = require("awful")
local beautiful = require("beautiful")
local notify = require("naughty").notify
local wibox = require("wibox")
local screenshot_dir = "/tmp/"
local gears = require("gears")
local module = {}

local function cmd_exists(cmd)
	for dir in string.gmatch(os.getenv("PATH"), "([^:]+)") do
		if gears.filesystem.file_executable(dir .. "/" .. cmd) then return true end
	end
	return false
end

local function screenshot(action)
	local file = string.format("/tmp/Screenshot - %s.png", os.date("%Y-%m-%d - %H:%M:%S"))
	local cmd
	if cmd_exists("import") then
		cmd = {
			["whole"] = "import -window root",
			["partial"] = "import"
		}
	elseif cmd_exists("scrot") then
		cmd = {
			["whole"] = "scrot",
			["partial"] = "scrot -fs"
		}
	else
		notify {
			title = "No Screenshot Tool Found",
			text = "The commands 'import' and 'scrot' are supported"
		}
		return
	end

	awful.spawn.easy_async_with_shell(
		string.format("sleep 0.05; %s '%s'", cmd[action], file),
		function(stdout, stderr, reason, exit_code)
			if exit_code ~= 0 then return end
			notify {
				title = "Took Screenshot",
				text = "As " .. file,
			}
		end
	)
end


local opts = {
	layout = wibox.layout.grid,
	homogeneous = true,
	expand = false,
	forced_num_cols = 1,
	{
		text = "Whole Screen",
		func = function() screenshot("whole") end
	},
	{
		text = "Partial",
		func = function() screenshot("partial") end
	},
	{
		text = "Cancel",
		func = nil
	}
}

for i, v in ipairs(opts) do
	opts[i] = {
		widget = wibox.container.background,
		shape = gears.shape.rounded_rect,
		id = "option",
		func = function()
			if v.func then v.func() end
			module.toggle()
		end,
		{
			widget = wibox.container.margin,
			margins = 20,
			{
				widget = wibox.widget.textbox,
				text = v.text,
				align = "center"
			}
		}
	}
end


local function update(widgets, chosen)
	for i, w in ipairs(widgets) do
		if i == chosen then
			w.bg = "#00000030"
		else
			w.bg = nil
		end
	end
end

local function press(widgets, chosen)
	widgets[chosen].func()
end

local function choose_next(widgets)
	if module.chosen ~= #widgets then module.chosen = module.chosen + 1 end
	update(widgets, module.chosen)
end

local function choose_prev(widgets)
	if module.chosen ~= 1 then module.chosen = module.chosen - 1 end
	update(widgets, module.chosen)
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
		border_width = beautiful.border_width,
		border_color = beautiful.border_focus,
		placement = awful.placement.centered,
		shape = gears.shape.rounded_rect,
		ontop = true,
		widget = {
			widget = wibox.container.margin,
			margins = 20,
			opts
		}
	}

	local widgets = module.popup.widget:get_children_by_id("option")
	for i, w in ipairs(widgets) do
		w:connect_signal(
			"mouse::enter",
			function()
				module.chosen = i
				update(widgets, module.chosen)
			end
		)
		w:connect_signal(
			"button::press",
			function()
				press(widgets, module.chosen)
			end
		)
	end

	update(widgets, module.chosen)
	module.keygrabber = awful.keygrabber {
		autostart = true,
		keybindings = {
			{{}, "j", function() choose_next(widgets) end},
			{{}, "k", function() choose_prev(widgets) end},
			{{"Control"}, "n", function() choose_next(widgets) end},
			{{"Control"}, "p", function() choose_prev(widgets) end},
			{{"Control"}, "g", module.toggle},
			{{}, "Escape", module.toggle},
			{{}, "Return", function() press(widgets, module.chosen) end},
			{{"Control"}, "m", function() press(widgets, module.chosen) end}
		}
	}
end


return module
