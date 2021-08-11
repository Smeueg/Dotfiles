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
To enable the firefox theme, symlink ~/.config/browser/firefox to ~/.mozilla/firefox/xxxxxxxx.default-release/chrome:
```sh
ln -s $HOME/.config/browser/firefox  $HOME/.mozilla/firefox/*.default-release/chrome
```
Replace '*' with your profile directory if you don't want all of your profiles to have the theme


### Salt Mine
This one liner:
```sh
curl -L https://gitlab.com/Smeueg/dotfiles/-/raw/master/.local/bin/migrate/salt-mine | python
```
This _should_ work on both python3 and python2.


### "Dependencies"
These are all really optional, since without them, the "rice" would still be functional:
- hsetroot (Setting the wallpaper)
- xcompmgr or any other compositor for that matter. (For terminal transparency, change the alpha value in the st config.h)
- xset (Change autorepeat value)
- Patched libxft with the bgra patch for emojis on st


## TODO
### Important
* TRY OUT ALPINE LINUX
* GET CONNMAN TO WORK (also make a connmantui in shell script)

### Less Important but still important
- Create a better dotfiles management script
- Add support for connman and iwd
- Finish firefox theming (Add css for stackoverflow and it's siblings, gitlab, github, and reddit)
- Add a gtk theme
- Add a license
- Finish previewer


## Random Information
If you're using wayland and a chromium browser try this flag:
```
--enable-features=UseOzonePlatform --ozone-platform=wayland This works for any browser based on Chromium v87 (or more)
```
For firefox, add this environment variable:
```
MOZ_ENABLE_WAYLAND=1
```

## Recommendations (from myself)
1. Mount /tmp as tmpfs or zram. This reduces reads and writes to the HDD/SSD. (Keep in mind that on each reboot everything in /tmp will be deleted, if it hasn't already)
2. Also read about [zram](https://www.kernel.org/doc/html/latest/admin-guide/blockdev/zram.html)/zswap/zcache which will also reduce reads and writes to the HDD/SSD
3. For people that use github or gitlab, install [hub](https://github.com/profclems/glab) or [cli/gh](https://github.com/cli/cli), both of which are for github, or [lab](https://github.com/zaquestion/lab/) or [glab](https://github.com/profclems/glab) and both are for gitlab
4. If you want a bitmap font, [scientifica](https://github.com/NerdyPepper/scientifica) and [cozette](https://github.com/slavfox/Cozette) seems interesting

### Dual Booting Windows:
Don't... Windows is garbage

If you still are going to:
Either change linux to use localtime instead of UTC *or* change Windows to use utc instead of localtime.

[This Link](https://itsfoss.com/wrong-time-dual-boot/) (beware of the blinding light)
