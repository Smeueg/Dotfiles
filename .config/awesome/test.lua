local naughty = require("naughty")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local awful = require("awful")
local Gio = require("lgi").Gio
local animate = require("animate")
local pr = function(str)
	naughty.notify({ text = "" .. str })
end


animate.simple("f", 1, 5, 10, function(i) pr(i) end)
