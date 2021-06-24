export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}


# Add directories to PATH if they don't exist already
for dir in $(find $HOME/.local/bin/ -maxdepth 1 -type d) $HOME/.local/share/npm/bin; do
    echo "$PATH" | grep "$dir" >/dev/null || PATH="$PATH:$dir"
done
export PATH

export CCACHE_DIR=$HOME/.cache/ccache

export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export PYLINTHOME="$XDG_CACHE_HOME"/pylint

export TERMINAL="st"
export READER="zathura"
export BROWSER="firefox"
export EDITOR="nvim"


export GNUPGHOME="$XDG_DATA_HOME"/gnupg

export _JAVA_AWT_WM_NONREPARENTING=1
export ANDROID_PREFS_ROOT="$XDG_CONFIG_HOME"/android
export ANDROID_EMULATOR_HOME="$XDG_DATA_HOME"/android/emulator

export WINEPREFIX="$XDG_DATA_HOME"/wineprefixes/default
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Pass
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass

command -v qt5ct >/dev/null && export QT_QPA_PLATFORMTHEME=qt5ct # if qt5ct is installed
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc


if [ "$(tty)" = "/dev/tty1" ]; then
	printf "\033[1;32m:: \033[0m Start X Session \033[36m [Y/n] \033[0;0m"
	read -t 3 OPT

    case $(echo "$OPT" | grep -o "^.") in
		[Yy]|"") startx $HOME/.config/X11/xinitrc ;;
		*) echo "Will not run X server" ;;
	esac
fi

source $HOME/.bashrc
