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


# @section Prompt
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


# @section Aliases
export SMEUESIC_URL="https://www.youtube.com/playlist?list=PLRV1hc8TIW-7znQIWaVarxdUxf7lskmBc"
alias mkdir="mkdir -pv"
alias diff="diff --color=always"
alias less="less -r"
alias grep="grep --color=auto"
alias ls="ls --color --group-directories-first"
alias mv="mv -i"
alias rm="rm -i"
alias cp="cp -i"
alias ip="ip --color=auto"
alias dit='git --git-dir=${HOME}/.local/dots --work-tree=${HOME}'
alias sudo="sudo --preserve-env=TERMINFO"
alias protonvpn='{ [ "$(pidof nm-applet)" ] || nm-applet & } ; protonvpn-cli'



# @section Custom Functions/Commands
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
		emacsclient -a "${EDITOR}" "$@" &
	else
		${EDITOR} "$@"
	fi
}


# @description Displays all the supported colors
# @noargs
colors() {
    i=0
    while [ ${i} -le 15 ]; do
        printf '\033[48;5;%dm  \033[m' "${i}"
		[ $i -eq 7 ] && printf "\n"
        i=$((i + 1))
    done

	printf "\n"
    while [ ${i} -le 225 ]; do
        [ $(((i - 16) % 6)) -eq 0 ] && printf "\n"
        printf '\033[48;5;%dm  \033[m' "${i}"
        i=$((i + 1))
    done
	printf "\n"
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
