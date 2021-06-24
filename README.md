# Salty Dotfiles for Linux Ricing and Seasoning
These dotfiles are meant to also be used alongside my suckless forks:
- (dwm)[https://gitlab.com/FriedTeaCP/tea-dwm]
- (st)[https://gitlab.com/FriedTeaCP/tea-st]
- (dwmblocks)[https://gitlab.com/FriedTeaCP/tea-dwmblocks]
- (dmenu)[https://gitlab.com/FriedTeaCP/tea-dmenu]


- Neovim as the text editor
- CMUS (C Music Player) as the audio player
- Simple Terminal (ST) as the terminal emulator
- Tmux as a terminal multiplexer


## Using a git bare repository to manage your salty dotfiles
Refer to the arch wiki: https://wiki.archlinux.org/index.php/Dotfiles


## Installing/Migrating into a new system
### IN DEVELOMPENT

After migrating/installing, make sure to allow the user to be able to shutdown, reboot, and suspend without a password (Either configuring visudo for sudo or /etc/doas.conf for doas).

### "Dependencies"
These are all really optional, since without them, the "rice" would still be functional:
- hsetroot (Setting the wallpaper)
- xcompmgr or any other compositor for that matter. (For terminal transparency, change the alpha value in the st config.h)
- setxkbmap (To enable keypad mouse/pointer)
- xrandr (Change and grab the screen resolution)
- xset (Change autorepeat value)
- Patched libxft with the bgra patch for emojis on st


## TODO
- Make a dwm patch to add a bottom line of the statusbar
- Finish the migration script
- Recreate a color-pallete and remake themes (tmux, neovim, dwm, st, dmenu, firefox, cmus, ...)
- Show current session and all available sessions on tmux
- Fix Fonts (FiraCode vs Jetbrains Mono) (Maybe NerdFont)
- Create a better dotfiles management script
- Allow user to shutdown/reboot/suspend without password



## Recommendations (from myself)
1. Mount /tmp as tmpfs. This reduces reads and writes to the HDD/SSD. (Keep in mind that on each reboot everything in /tmp will be deleted)
2. Also read about [zram](https://www.kernel.org/doc/html/latest/admin-guide/blockdev/zram.html)/zswap/zcache which will also reduce reads and writes to the HDD/SSD
3. For people that use github or gitlab, install [hub](https://github.com/profclems/glab) or [cli/gh](https://github.com/cli/cli), both of which are for github, or [lab](https://github.com/zaquestion/lab/) or [glab](https://github.com/profclems/glab) and both are for gitlab
4. If you want a bitmap font, [scientifica](https://github.com/NerdyPepper/scientifica) and [cozette](https://github.com/slavfox/Cozette) seems interesting
5. Use hwclock to make sure that the system time and harware time is correct and the same.
