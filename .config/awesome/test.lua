local naughty = require("naughty")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local awful = require("awful")
local Gio = require("lgi").Gio
local pr = function(str)
	naughty.notify({ text = "" .. str })
end


local function foo()
	return
end

pr(debug.getinfo(foo).nparams)
