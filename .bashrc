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

	[ -f /usr/share/bash-completion/bash_completion ] &&
		. /usr/share/bash-completion/bash_completion
elif [ "${ZSH_NAME}" ]; then
	# Zsh Specific configuration
	setopt autocd
	bindkey -v '^H' backward-delete-char
	bindkey -v '^?' backward-delete-char
	setopt PROMPT_SUBST
fi



# PROMPT #
# Should work on dash, bash, zsh, and ksh
print_prompt() {
	ret=${?}
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
		unset reset color_dir color_branch color_failed color_success
	fi

	if ${color}; then
		reset="${wrapper_start}\033[0m${wrapper_end}"
		color_dir="${wrapper_start}\033[38;5;11m${wrapper_end}"
		color_branch="${wrapper_start}\033[38;5;4m${wrapper_end}"
		color_failed="${wrapper_start}\033[38;5;1m${wrapper_end}"
		color_success="${wrapper_start}\033[38;5;6m${wrapper_end}"
	fi

	tmp="$(tty)"


	if [ "${wrapper+x}" ]; then
		for var in reset color_dir color_branch color_failed color_success; do
			eval "${var}=\"\${wrapper_start}\$${var}\${wrapper_end}\""
		done
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
	branch=$(git branch --show-current 2>/dev/null)
	[ "${branch}" ] && printf '%b [ %s ]' "${color_branch}" "${branch}"

	# Symbol color
	if [ ${ret} -eq 0 ]; then
		printf '\n%b%s%b ' "${color_success}" "${symbol}" "${reset}"
	else
		printf '\n%b%s%b ' "${color_failed}" "${symbol}" "${reset}"
	fi
}
export PS1="\$(print_prompt)"



# ALIASES #
alias emacs="emacs -nw"
alias mkdir="mkdir -pv"
alias diff="diff --color=always"
alias grep="grep --color=auto"
alias ls="ls --color --group-directories-first"
alias mv="mv -i"
alias rm="rm -i"
alias cp="cp -i"
alias ip="ip --color=auto"
alias dit="git --git-dir=${HOME}/.local/dots --work-tree=${HOME}"
alias smeuesic="sh <(curl -sNL https://raw.githubusercontent.com/Smeueg/ytpl-sync/main/ytpl-sync) --url 'https://www.youtube.com/playlist?list=PLRV1hc8TIW-7znQIWaVarxdUxf7lskmBc'"
alias ytpl="sh <(curl -sNL https://raw.githubusercontent.com/Smeueg/ytpl-sync/main/ytpl-sync)"
alias get_rustup="curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs|sh"
alias sudo="sudo --preserve-env=TERMINFO"

# FUNCTIONS #
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


fix_time() {
	# Fix time desync in Linux
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


jmtpfs_auto_mount() {
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
		jmtpfs "/tmp/jmtpfs"
	else
		umount "/tmp/jmtpfs"
		rmdir "/tmp/jmtpfs"
	fi
}


sx() {
	# Start x server
	if [ -f "${HOME}/.config/X11/xinitrc" ]; then
		startx "${HOME}/.config/X11/xinitrc"
	else
		startx
	fi
}


e() {
	if [ "${EDITOR}" = "emacs" ] && [ "$(command -v emacsclient)" ]; then
		emacsclient -a ${EDITOR} $@ &
	else
		${EDITOR} $@
	fi
}


fetch() (
	# Get the OS
	for f in /etc/*-release; do
		read os < ${f}
		os=${os%%\"}
		os=${os##*\"}
	done

	# Get the terminal
	if [ "${TERM}" = "linux" ]; then
		term=$(tty)
	else
		term=$(ps --pid $PPID -o cmd -h)
		term=${term%% *}
	fi

	# The username and hostname
	name="\033[1;33m${USER}\033[0m@\033[33m${HOSTNAME}\033[0m\n"

	# String to seperate the user_info and all the other informations
	underline=""
	i=0
	while [ ${i} -ne $((${#USER} + ${#HOSTNAME} + 1)) ]; do
		underline="${underline}─"
		i=$((i + 1))
	done

	# String to print colors
	colors=""
	i=0
	while [ ${i} -ne 7 ]; do
		colors="${colors}\033[3${i}m●\033[0m "
		i=$((i + 1))
	done

	# Actual printing
	printf "┌────────────────┐\n"
	printf "│                │ ${name}"
	printf "│   ·  ·   ·  ·  ├─${underline}─\n"
	printf "│   ·      ●     │ \033[1;31mOS\033[0m      ─  ${os}\n"
	printf "│      ·      ·  │ \033[1;33mTERM\033[0m    ─  ${term##*/}\n"
	printf "│  ·     ·  ·    │ \033[1;34mSHELL\033[0m   ─  ${SHELL##*/}\n"
	printf "│    ● ·  ·   ·  │ \033[1;35mEDITOR\033[0m  ─  ${EDITOR}\n"
	printf "│     ·      ·   │ \033[1;36mKERNEL\033[0m  ─  $(uname -r)\n"
	printf "│                │ ${colors}\n"
	printf "└────────────────┘\n"
)


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



# MISC #
# Git
if [ "$(command -v git)" ]; then
	[ "$(git config --get-regexp ^alias\.graph)" ] ||
		git config --global alias.graph "log --graph --oneline"
fi

# Rust
[ -f "${HOME}/.local/share/cargo/env" ] &&
	. "${HOME}/.local/share/cargo/env"

# Less
export LESS_TERMCAP_mb=$'\033[1;32m'
export LESS_TERMCAP_md=$'\033[1;32m'
export LESS_TERMCAP_so=$'\033[1;33m'
export LESS_TERMCAP_me=$'\033[0m'
export LESS_TERMCAP_se=$'\033[0m'
export LESS_TERMCAP_ue=$'\033[0m'
export LESS_TERMCAP_us=$'\033[1;4;31m'


if ! [ "${TERM}" = "linux" ]; then
	printf "\033[H\033[J%b\n"
	fetch
fi
