#!/bin/sh
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_STATE_HOME=${XDG_STATE_HOME:="$HOME/.local/state"}
export XDG_DESKTOP_DIR="${HOME}"
export XCURSOR_PATH=/usr/share/icons:$XDG_DATA_HOME/icons
export XAUTHORITY="${XDG_RUNTIME_DIR}"/Xauthority

# Add directories to PATH if they don't exist already
for dir in /sbin/ "${HOME}"/.local/bin/ "${HOME}"/.local/share/cargo/bin; do
	[ "${PATH##*${dir%/}*}" ] || continue
	PATH="${dir%/}:${PATH}"
done
export PATH


export EDITOR=emacs

export LESSHISTFILE=/tmp/less-hist
export CCACHE_DIR=${HOME}/.cache/ccache
export TERMINFO="$XDG_DATA_HOME"/terminfo
export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo

export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc
export PYLINTHOME="$XDG_CACHE_HOME"/pylint

export WINIT_X11_SCALE_FACTOR=1

# Fix some gnupg messed up permission or something, idk
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
if [ ! -d "${GNUPGHOME}" ]; then
	mkdir -pv "${GNUPGHOME}"
	chmod 700 "${GNUPGHOME}" # Set 700 for directories
fi

# Rust
export RUSTUP_HOME="${HOME}/.local/share/rustup"

# Java
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java

# Python
export PYTHON_HISTORY=${XDG_STATE_HOME}/python/history
export PYTHONPYCACHEPREFIX=${XDG_CACHE_HOME}/python
export PYTHONUSERBASE=${XDG_DATA_HOME}/python


# Android
export ANDROID_PREFS_ROOT="$XDG_CONFIG_HOME"/android
export ANDROID_EMULATOR_HOME="$XDG_DATA_HOME"/android/emulator

# Wget
export WGETRC="${XDG_CONFIG_HOME}/wgetrc"

# Zoom
export SSB_HOME="$XDG_DATA_HOME"/zoom

# Wine
export WINEPREFIX="$XDG_DATA_HOME"/wineprefixes/default

# Language/Locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Pass
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass

# Gtk & Qt
[ "$(command -v qt5ct)" ] && export QT_QPA_PLATFORMTHEME=qt5ct # if qt5ct is installed
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc

# Shell
export ZDOTDIR="${HOME}/.config/shellconfig"

# Cargo
export CARGO_HOME="${XDG_DATA_HOME}"/cargo

if [ "$(command -v simplescreenrecorder)" ]; then
	mkdir -vp "${XDG_CONFIG_HOME}/simplescreenrecorder/"
fi

while ! [ "${DISPLAY}" ] && [ "$(command -v startx)" ]; do
    printf "Start Xorg? [\033[32mY\033[m/\033[31mn\033[m] "
    read response
    case ${response} in
        [Yy]|[Yy][Ee][Ss]|"")
            if [ -f "${HOME}/.config/X11/xinitrc" ]; then
                startx "${HOME}/.config/X11/xinitrc"
            else
                startx
            fi
            ;;
        [Nn]|[Nn][Oo])
            printf "Won't start Xorg\n"
            break
            ;;
        *)
            printf "Invalid Response"
            ;;
    esac
done

[ "${DISPLAY}" ] || . ${HOME}/.bashrc
