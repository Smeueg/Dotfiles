#!/bin/sh
# Basic interactive shell config that should work on every posix compliant shell
set -o vi   # Vi mode, works on most shells

if [ -d "/tmp" ] && [ -w "/tmp" ]; then
    export HISTFILE=/tmp/shell_history
fi


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

# @section Python Automatic VEnv Activation
# shellcheck source=/dev/null
auto_toggle_venv() {
    python_cmd=
    if [ "$(command -v python3)" ]; then
        python_cmd="python3"
    elif [ "$(command -v python)" ]; then
        python_cmd="python"
    fi

    if ! [ "${python_cmd}" ]; then
        return
    fi


    if [ "$(command -v deactivate)" ]; then
        # If inside a venv
        project_directory=${VIRTUAL_ENV%/}
        project_directory=${project_directory%/.venv}
        if [ "${PWD%${project_directory}*}" ]; then
            deactivate
            printf "\033[1;32m|\033[m Exiting out of project %s, deactivated venv\n" "${project_directory##*/}"
        fi
    else
        # If *not* inside a venv
        dir=${PWD%/}
        while [ "${dir}" ]; do
            venv_activate_path="${dir}/.venv/bin/activate" 
            if [ -f "${venv_activate_path}" ]; then
                printf "\033[1;32m|\033[m Entering project %s, activated venv\n" "${dir##*/}"
                . "${venv_activate_path}"
                break
            fi

            dir=${dir%/}
            dir=${dir%/*}
        done
    fi
}

export PROMPT_COMMAND="auto_toggle_venv"


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
alias sudo="sudo --preserve-env=TERMINFO env \"PATH=\$PATH\""
alias protonvpn='{ [ "$(pidof nm-applet)" ] || nm-applet & } ; protonvpn-cli'
alias e="setsid \${EDITOR}"

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

# @section Change ${EDITOR} when inside an emacs shell
if [ "${TERM}" = "eat-truecolor" ]; then
	export EDITOR="emacsclient"
fi

#@section Fix GPG agent issues in TERMUX
if [ "${TERMUX_VERSION}" ]; then
    export GPG_TTY=$(tty)
fi
