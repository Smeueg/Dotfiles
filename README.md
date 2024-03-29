<h1>Very Smeueg™ Dotfiles For Linux</h1>
<div align="center"><img src="https://smeueg.github.io/pfp.png" height=128 width=128></div>
<div align="center"><img src="https://smeueg.github.io/rice.png" height=512 width=512></div>

---

## Programs
These dotfiles consists of configuration for:
- [AwesomeWM](https://awesomewm.org/)
- [Emacs](https://emacs.org/)
- And any POSIX compliant shell. (Screw you [Fish](https://fishshell.com/))

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

---

## Interesting Color Schemes
I'm currently using gruvbox, however I am interested in checking out some other themes, primarily:
- [Kanagawa](https://github.com/rebelot/kanagawa.nvim)
- [base16-default-dark](https://github.com/tinted-theming/base16-emacs)
