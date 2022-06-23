local beautiful = require("beautiful")
local naughty = require("naughty")
local cairo = require("lgi").cairo
local gears = require("gears")
local dpi = beautiful.xresources.apply_dpi
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

-- Titlebar Shapes --
local button_close = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local button_maximize = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
local button_minimize = cairo.ImageSurface.create(cairo.Format.ARGB32, 20, 20)
do
	local cr
	local transform = gears.shape.transform
	local shape = gears.shape
	cr = cairo.Context(button_close)
	cr:set_source(gears.color(theme["red"]))
	transform(shape.rectangle)
		:translate(5, 6)
		:rotate(math.pi/4)(cr, 10, 2)
	transform(shape.rectangle)
		:translate(11, 6)
		:rotate(math.pi/4)(cr, 2, 10)
	cr:fill()

	cr = cairo.Context(button_maximize)
	cr:set_source(gears.color(theme["green"]))
	transform(shape.rounded_rect)
		:translate(6.25, 6.25)(cr, 7.5, 7.5, 1)
	cr:stroke()

	cr = cairo.Context(button_minimize)
	cr:set_source(gears.color(theme["yellow"]))
	transform(shape.rounded_rect):translate(7.5, 8.75)(cr, 7.5, 2.5, 1)
	cr:fill()
end

-- Theme Variables --
beautiful.init()
beautiful.wallpaper = theme["wallpaper"]
beautiful.fg_normal = theme["fg"]
beautiful.fg_focus = theme["yellow"]
beautiful.bg_normal = theme["bg"]
beautiful.font = theme["font"]
-- Wibar
beautiful.wibar_selected_tag = theme["fg"]
beautiful.wibar_unselected_tag = theme["fg2"]
beautiful.wibar_bg = beautiful.bg_normal
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
beautiful.titlebar_bg = beautiful.bg_normal
beautiful.titlebar_bg_focus = theme["bg_light"]
-- Prompt
beautiful.prompt_bg_cursor = beautiful.fg_normal
-- Tooltips
beautiful.tooltip_bg = theme["bg_light"]
beautiful.tooltip_fg = theme["fg"]
beautiful.tooltip_border_color = theme["focus"]
beautiful.tooltip_border_width = 1
-- Menu Bar
beautiful.menubar_fg_focus = theme["focus"]
beautiful.menubar_bg_focus = theme["bg_light"]
beautiful.menubar_fg_normal = beautiful.fg_normal
-- Borders
beautiful.border_normal = theme["bg"]
beautiful.border_focus = theme["focus"]
beautiful.border_width = dpi(4)
beautiful.maximized_hide_border = true
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
-- Tasklist
beautiful.tasklist_shape = gears.shape.rounded_rect
beautiful.tasklist_bg_focus = theme["bg_dark"]
beautiful.tasklist_bg_normal = theme["bg"]
beautiful.tasklist_bg_minimize = beautiful.wibar_bg
-- Notification
beautiful.notification_border_color = theme["focus"]
naughty.config.defaults.border_width = 3
naughty.config.defaults.margin = 10
naughty.config.defaults.timout = 3
naughty.config.spacing = 10
