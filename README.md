# Salty Dotfiles for Linux Ricing and Seasoning
These dotfiles are meant to also be used alongside my suckless forks:
- (dwm)[https://gitlab.com/Smeueg/dwm]
- (st)[https://gitlab.com/Smeueg/st]
- (dwmblocks)[https://gitlab.com/Smeueg/dwmblocks]
- (dmenu)[https://gitlab.com/Smeueg/dmenu]

- Neovim as the text editor
- Tmux as a terminal multiplexer
And some other stuff


## Using a git bare repository to manage your salty dotfiles
Refer to the arch wiki: https://wiki.archlinux.org/index.php/Dotfiles


## Installing/Migrating into a new system
### Salt Mine
This one liner:
```sh
curl -L https://gitlab.com/Smeueg/dotfiles/-/raw/master/.local/bin/salt-mine | python
```

This _should_ work on both python3 and python2.


### "Dependencies"
These are all really optional, since without them, the "rice" would still be functional:
- hsetroot (Setting the wallpaper)
- xcompmgr or any other compositor for that matter. (For terminal transparency, change the alpha value in the st config.h)
- xset (Change autorepeat value)
- Patched libxft with the bgra patch for emojis on st


## TODO
- Create a better dotfiles management script
- Add a license


## Recommendations (from myself)
1. Mount /tmp as tmpfs or zram. This reduces reads and writes to the HDD/SSD. (Keep in mind that on each reboot everything in /tmp will be deleted, if it hasn't already)
2. Also read about [zram](https://www.kernel.org/doc/html/latest/admin-guide/blockdev/zram.html)/zswap/zcache which will also reduce reads and writes to the HDD/SSD
3. For people that use github or gitlab, install [hub](https://github.com/profclems/glab) or [cli/gh](https://github.com/cli/cli), both of which are for github, or [lab](https://github.com/zaquestion/lab/) or [glab](https://github.com/profclems/glab) and both are for gitlab
4. If you want a bitmap font, [scientifica](https://github.com/NerdyPepper/scientifica) and [cozette](https://github.com/slavfox/Cozette) seems interesting

### Dual Booting Windows:
Don't... Windows is garbage

If you still are going to, shame on you:
Either change linux to use localtime instead of UTC *or* change Windows to use utc instead of localtime.

[This Link](https://itsfoss.com/wrong-time-dual-boot/) (beware of the blinding light)
