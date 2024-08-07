#!/bin/sh
# Basic interactive shell config that should work on every posix compliant shell
set -o vi   # Vi mode, works on most shells
export HISTFILE=/tmp/shell_history


# Shell Options #
if [ "${BASH_VERSION}" ]; then
	# Bash Specific configuration
	complete -c man which
	complete -cf sudo
	shopt -s autocd
	shopt -s checkwinsize
    bind 'set bell-style none'

	[ -f /usr/share/bash-completion/bash_completion ] &&
		. /usr/share/bash-completion/bash_completion
elif [ "${ZSH_NAME}" ]; then
	# Zsh Specific configuration
	setopt autocd
	bindkey -v '^H' backward-delete-char
	bindkey -v '^?' backward-delete-char
	setopt PROMPT_SUBST
fi


# Prompt #
# Should work on dash, bash, zsh, and ksh
print_prompt() {
	ret=$?
    symbol=">"
    color=true
    if [ "${BASH_VERSION}" ]; then
	    wrapper_start="\001"
	    wrapper_end="\002"
    elif [ "${ZSH_NAME}" ]; then
	    wrapper_start="%{"
	    wrapper_end="%}"
    elif ! [ "${KSH_VERSION}" ]; then
	    color=false
    fi

    if ${color}; then
	    reset="${wrapper_start}\033[0m${wrapper_end}"
	    color_dir="${wrapper_start}\033[38;5;11m${wrapper_end}"
	    color_branch="${wrapper_start}\033[38;5;4m${wrapper_end}"
	    color_failed="${wrapper_start}\033[38;5;1m${wrapper_end}"
	    color_success="${wrapper_start}\033[38;5;6m${wrapper_end}"
    fi

    # Change the directory color to red if the user doesn't have permissions to
    # write to the directory
    [ -w "${PWD}" ] || color_dir=${color_failed}

    # Current Directory
    if ! [ "${PWD%${HOME}*}" ]; then
	    printf '%b%s' "${color_dir}" "~${PWD#${HOME}}"
    else
	    printf '%b%s' "${color_dir}" "${PWD}"
    fi

    # Git Branch
    branch=$(git branch --show-current 2>&1)
    [ $? = 0 ] && printf '%b [ %s ]' "${color_branch}" "${branch}"

    # Symbol color
    if [ ${ret} -eq 0 ]; then
	    printf '\n%b%s%b ' "${color_success}" "${symbol}" "${reset}"
    else
	    printf '\n%b%s%b ' "${color_failed}" "${symbol}" "${reset}"
    fi
}
export PS1="\$(print_prompt)"


# Aliases #
alias mkdir="mkdir -pv"
alias diff="diff --color=always"
alias less="less -r"
alias grep="grep --color=auto"
alias ls="ls --color --group-directories-first"
alias mv="mv -i"
alias rm="rm -i"
alias cp="cp -i"
alias ip="ip --color=auto"
alias dit="git --git-dir=${HOME}/.local/dots --work-tree=${HOME}"
alias smeuesic="sh <(curl -sNL https://raw.githubusercontent.com/Smeueg/ytpl-sync/main/ytpl-sync) --url 'https://www.youtube.com/playlist?list=PLRV1hc8TIW-7znQIWaVarxdUxf7lskmBc'"
alias ytpl="sh <(curl -sNL https://raw.githubusercontent.com/Smeueg/ytpl-sync/main/ytpl-sync)"
alias sudo="sudo --preserve-env=TERMINFO"
alias protonvpn='{ [ "$(pidof nm-applet)" ] || nm-applet & } ; protonvpn-cli'



# @section Custom Functions/Commands


# @description A wrapper around `droidcam-cli` to automatically use audio
# @noargs
dra() {
	if ! [ "$(command -v droidcam-cli)" ]; then
		printf "\033[1;31m|\033[0m 'droidcam-cli' isn't installed\n" >&2
		return 127
	fi

	for arg in ${@}; do
		if [ "${arg}" = "-a" ]; then
			{
				while sleep 1; do [ "$(ps -c droidcam-cli)" ] && break; done
				if [ "$(pactl list short | grep "DroidcamAudio")" ]; then
					pactl load-module module-alsa-source \
						  device=hw:Loopback,1,0 \
						  source_properties=device.description=DroidcamAudio
					if [ ${?} = 0 ]; then
						printf "\033[1;32m|\033[0m Loaded pulseaudio module\n"
					else
						printf "\033[1;31m|\033[0m Failed to load pulseaudio module\n" >&2
					fi
				fi
				pactl set-default-source "alsa_input.hw_Loopback_1_0"
				pactl set-source-volume @DEFAULT_SINK@ 150%
			} &
			break
		fi
	done

	droidcam-cli ${@}
}


# @description Fix time desync in Linux
# @noargs
fix_time() {
	err=false
	for dep in ntpd hwclock sudo; do
		[ "$(command -v ${dep})" ] && continue
		printf "\033[1;31m|\033[0m Command '${dep}' not found\n" >&2
		err=true
	done
	${err} && return 1
	printf "Running 'ntpd -qg; hwclock --systohc'\n"
	sudo "${SHELL}" -c  'ntpd -qg; hwclock --systohc'
}


# @description Automatically mount a phone's filesystem using `jmtpfs`
# @noargs
jmtpfs_auto() {
	if ! [ "$(command -v jmtpfs)" ]; then
		printf "\033[1;31m|\033[0m Command 'jmtpfs' not found\n" >&2
		return 1
	fi

	if ! jmtpfs -l | { read _; read line; [ "${line}" ]; }; then
		printf "\033[1;31m|\033[0m No devices found\n" >&2
		return 1
	fi

	if ! [ -d "/tmp/jmtpfs" ]; then
		mkdir "/tmp/jmtpfs"
		jmtpfs "/tmp/jmtpfs"\
			   -o direct_io\
			   -o no_remote_lock\
			   -o use_ino\
			   -o default_permissions
		[ $? -ne 0 ] && rmdir "/tmp/jmtpfs"
	else
		umount "/tmp/jmtpfs"
		rmdir "/tmp/jmtpfs"
	fi
}


# @description A shortcut to start the X server using `xinitrc`
# @noargs
sx() {
	if [ -f "${HOME}/.config/X11/xinitrc" ]; then
		startx "${HOME}/.config/X11/xinitrc"
	else
		startx
	fi
}

# @description A shortcut to edit using `emacsclient` if $EDITOR is `emacs`
# @noargs
e() {
	if [ "${EDITOR}" = "emacs" ] && [ "$(command -v emacsclient)" ]; then
		emacsclient -a ${EDITOR} $@ &
	else
		${EDITOR} $@
	fi
}


# @description Automatically mount rclone
# @noargs
rcmnt() {
	if ! [ "$(command -v rclone)" ]; then
		printf "\033[1;31m|\033[0m Command 'rclone' not found\n" >&2
		return 1
	fi

	remotes=$(rclone listremotes); i=0
	while read remote; do
		i=$((i + 1))
		printf "  %d. %s\n" ${i} "${remote%:}"
	done <<-EOF
	${remotes}
	EOF

	printf "Select Remote: "; read remote_id
	case ${remote_id} in
		*[!0-9]*|"")
			printf "\033[1;31m|\033[0m Not a number\n" >&2
			exit 1
			;;
	esac

	if [ ${remote_id} -gt ${i} ] || [ ${remote_id} -lt 1 ]; then
		printf "\033[1;31m|\033[0m Not a valid remote\n" >&2
		return 1
	fi

	i=0
	while [ ${i} -ne ${remote_id} ]; do
		i=$((i + 1))
		read remote
	done <<-EOF
	${remotes}
	EOF

	if mkdir -pv "/tmp/${remote%:}"; then
		printf "Mounted ${remote%:}\n"
		rclone mount "${remote}" "/tmp/${remote%:}" &
	fi
}


# @section Configuration for `git`
if [ "$(command -v git)" ]; then
	[ "$(git config --get-regexp ^alias\.graph)" ] ||
		git config --global alias.graph "log --graph --oneline"
	if [ -f "${HOME}/.gitconfig" ] && [ -d "${XDG_CONFIG_HOME}" ]; then
		[ -d "${XDG_CONFIG_HOME}/git/" ] || mkdir -p "${XDG_CONFIG_HOME}/git/"
		mv -f "${HOME}/.gitconfig" "${XDG_CONFIG_HOME}/git/config"
	fi
fi

# @section Configuration for `cargo`
[ -f "${HOME}/.local/share/cargo/env" ] &&
	. "${HOME}/.local/share/cargo/env"

# @section Configuration for `less`
export LESS_TERMCAP_mb=$'\033[1;32m'
export LESS_TERMCAP_md=$'\033[1;32m'
export LESS_TERMCAP_so=$'\033[1;33m'
export LESS_TERMCAP_me=$'\033[m'
export LESS_TERMCAP_se=$'\033[m'
export LESS_TERMCAP_ue=$'\033[m'
export LESS_TERMCAP_us=$'\033[1;4;31m'


# @section Give warnings based on XDG_BASE_DIRECTORY compliance
# Emacs
if [ -f "${XDG_CONFIG_HOME}/emacs/init.el" ]; then
    for file in "${HOME}/.emacs" "${HOME}/.emacs.d"; do
        { [ -f "${file}" ] || [ -d "${file}" ]; } || continue
        printf "%b: '%s' exists but '%s' is used instead\n" \
               "\033[1;31mWARN\033[m" \
               "${XDG_CONFIG_HOME}/emacs/init.el" \
               "${file}"
    done
fi
