# Salty Dotfiles for Linux Ricing and Seasoning
## These dotfiles can also be used alongside my suckless forks, although I have now prefer Awesome WM as my desktop of choice:
## Things I use:
- AwesomeWM
- Emacs
- Bash

## Things I used to use (Configurations still exists but I don't edit and use them anymore):
- Neovim (0.5+)
- Vim
- Tmux
- [dwm](https://gitlab.com/Smeueg/dwm)
- [st](https://gitlab.com/Smeueg/st)
- [dwmblocks](https://gitlab.com/Smeueg/dwmblocks)
- [dmenu](https://gitlab.com/Smeueg/dmenu)

![Screenshot](https://gitlab.com/Smeueg/Dotfiles/-/raw/master/.local/rice/misc/Screenshot.png)


## Using a git bare repository to manage your salty dotfiles
Refer to the arch wiki: https://wiki.archlinux.org/index.php/Dotfiles


## Installing/Migrating into a new system
### Smeueger
This is how you can become more smeueg *INSTANTLY* using this one liner:
```sh
curl -L https://gitlab.com/Smeueg/dotfiles/-/raw/master/.config/scripts/migrate/smeueger | sh
```
This _should_ be able to work with most shells (except for fish).

### Firefox Theme
To enable the firefox theme, symlink (soft link) ~/.config/browser/firefox to ~/.mozilla/firefox/xxxxxxxx.default-something/chrome:
```sh
ln -s $HOME/.config/browser/firefox  $HOME/.mozilla/firefox/*.default-*/chrome
```
Replace '*' with your profile directory if you don't want all of your profiles to have the theme


### "Dependencies"
- Emacs
- Awesomewm
- A shell (A posix compliant one, *NOT* fish)

These are all really optional, since without them, the "rice" would still be functional:
- xset (Change autorepeat value)
- xrandr (To change the resolution)
- xrdb (To set xresources values)

These are the dependencies I usually use with my suckless configuration:
- hsetroot (Setting the wallpaper)
- xcompmgr or any other compositor for that matter. (For terminal transparency, change the alpha value in the st config.h)
- Patched libxft with the bgra patch for emojis on st


## TODO
### Important
* make a connmantui in shell script

### Less Important but still important
- Create a better dotfiles management script
- Add a gtk theme
- Add a license


## Random Information
### Browsers in Wayland
If you're using wayland and a chromium browser try this flag:
```
--enable-features=UseOzonePlatform --ozone-platform=wayland This works for any browser based on Chromium v87 (or more)
```
For firefox, add this environment variable:
```
MOZ_ENABLE_WAYLAND=1
```

### Alacritty with vim/nvim
If you're having problems about alacritty with vim (alacritty -e vim somefile), the 'fix' is to launch alacritty, then wait a second (or a couple of miliseconds), then launch vim.

for example:
```
dir="${HOME}"/.config/scripts
alacritty -e ${SHELL} -c "sleep 0.2; vim ${dir}/$(cd "${dir}"; find * -type f | dmenu)"
```
Notice the sleep 0.2, you might need to increase the time if there's still problems, or decrease it if you feel like the waiting/loading time is too slow.
If you have $TERMINAL set or $EDITOR set, you could probably replace alacritty with $TERMINAl and vim with $EDITOR


## Recommendations (from myself)
1. Mount /tmp as tmpfs or zram. This reduces reads and writes to the HDD/SSD. (Keep in mind that on each reboot everything in /tmp will be deleted, if it hasn't already)
2. Also read about [zram](https://www.kernel.org/doc/html/latest/admin-guide/blockdev/zram.html)/zswap/zcache which will also reduce reads and writes to the HDD/SSD
3. For people that use github or gitlab, install [hub](https://github.com/profclems/glab) or [cli/gh](https://github.com/cli/cli), both of which are for github, or [lab](https://github.com/zaquestion/lab/) or [glab](https://github.com/profclems/glab) and both are for gitlab
4. If you want a bitmap font, [scientifica](https://github.com/NerdyPepper/scientifica) and [cozette](https://github.com/slavfox/Cozette) seems interesting
5. Try out the ultimate recipe which I call the bbbccccdhhjpst sandwhich (bacon beef beans chicken chili cheese chili flakes duck ham hot sauce jalapenos pork sausage turkey sandwich)

### Distro Specific:
#### Alpine:
Do not forget to install elogind, linux-firmware and add your normal user to the video and input group

### Dual Booting Windows:
Don't... Windows is garbage

If you still are going to:
Either change linux to use localtime instead of UTC *or* change Windows to use utc instead of localtime.

[This Link](https://itsfoss.com/wrong-time-dual-boot/) (beware of the blinding light)
