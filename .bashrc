complete -c man which
complete -cf sudo
shopt -s autocd # Change directories without typing cd
shopt -s checkwinsize
stty -ixon 		# Disable Ctrl-q and Ctrl-s
set -o vi 		# Vi mode
export HISTFILE=$XDG_CACHE_HOME/bash_history
export LESSHISTFILE=/dev/null


# Prompt
Prompt() {
	dirCol=3
	branchCol=11
	PS1="\[\033[1m\]\[\033[38;5;${dirCol}m\]\w"

	# Git branch + symbol color depending on return value of previous command
	PS1+="\[\033[1m\]\[\033[38;5;${branchCol}m\]"
	PS1+="\$(
		ret=\$?
		printf \"\$(git branch 2>&1 | sed '/^[^*]/d; s/* \(.*\)$/ [\1]/')\"
		[ \$ret = 0 ] && printf '\n\[\033[38;5;11m\]' ||  printf '\n\[\033[38;5;1m\]'
	)"

	PS1+='\[\033[1m\]> '
	PS1+='\[\033[0m\]'
	export PS1

	unset dirCol
	unset branchCol
}
Prompt
unset Prompt


if [ -d "${HOME}"/.config/shellconfig ]; then
	for f in ~/.config/shellconfig/*; do
		source "$f"
	done
fi


if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
fi


true
