<h1>Very Smeueg™ Dotfiles For Linux</h1>
<div align="center">
<img src="./.config/rice/Smeueg.png" height=128 width=128>
<img src="./.config/rice/Screenshot.png" height=512 width=512>
</div>

---

## Programs
These dotfiles consists of configuration for:
- [AwesomeWM](https://awesomewm.org/)
- [Emacs](https://emacs.org/)
- [Rofi](https://github.com/davatorium/rofi) (Optional, AwesomeWM has it's own "menu")
- And any POSIX compliant shell. (Screw you [Fish](https://fishshell.com/))

There are also configuration files for programs that I used to use but no longer
do like:
- [Neovim](https://neovim.io/) and [Vim](https://www.vim.org/)
- [Tmux](https://github.com/tmux/tmux)

I no longer use any suckless utilities and as such, my forks of those are now
deleted. When I do try it out again, I will use:
- [dwm-flexipatch](https://github.com/bakkeby/dwm-flexipatch)
- [st-flexipatch](https://github.com/bakkeby/st-flexipatch)
- [dmenu-flexipatch](https://github.com/bakkeby/dmenu-flexipatch)
- [slock-flexipatch](https://github.com/bakkeby/slock-flexipatch)


## Dependencies
I categorized these as "dependencies" rather than "programs" because I don't
really use them interactively as much as something like my window manager or
emacs. They're mostly X utilities that IMO should be able to be controled from
the WM:
- xrandr (To change the resolution)
- xset (Change autorepeat value)
- xrdb (To set xresources values)

---

## Bootstraping
If you're using something like **bash** or **zsh**, you can bootstrap my
dotfiles by running:
```
sh <(curl -Ls https://smeueg.github.io/smeueger)
```
or
```
sh <(wget -qO- https://smeueg.github.io/smeueger)
```

I also have a script called "smeuesic" that's in this dotfiles repo that
syncs music from a playlist over on youtube. It'll also delete files that
aren't in the playlist so be carefull with that. You can run it with:
```
sh <(curl -Ls https://smeueg.github.io/smeuesic)
```
or
```
sh <(wget -qO- https://smeueg.github.io/smeuesic)
```
