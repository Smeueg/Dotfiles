complete -c man which
complete -cf sudo
shopt -s autocd # Change directories without typing cd
shopt -s checkwinsize
stty -ixon 		# Disable Ctrl-q and Ctrl-s
set -o vi 		# Vi mode
export HISTFILE=/home/$USER/.cache/bash_history
export LESSHISTFILE=-


# Prompt
Prompt() {
	PS1='\[\033[1;36m\]\w'

	# Git branch + symbol color depending on return value of previous command
	PS1+='\[\033[1;32m\]'
	PS1+="\$(
		ret=\$?
		printf \"\$(git branch 2>&1 | sed '/^[^*]/d; s/* \(.*\)$/ [\1]/')\"
		[ \$ret = 0 ] && printf '\n\[\033[32m\]' || printf '\n\[\033[31m\]'
	)"

	PS1+='> '
	PS1+='\[\033[0m\]'
	export PS1
}
Prompt
unset Prompt



# Source seperate bash config file
for f in ~/.config/shellconfig/*; do source "$f"; done

# Source bash completion
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] &&
    . /usr/share/bash-completion/bash_completion

# "Less" coloring
lessColors() {
	export LESS_TERMCAP_mb=$'\033[1;32m'
	export LESS_TERMCAP_md=$'\033[1;32m'
	export LESS_TERMCAP_me=$'\033[0m'
	export LESS_TERMCAP_se=$'\033[0m'
	export LESS_TERMCAP_so=$'\033[01;33m'
	export LESS_TERMCAP_ue=$'\033[0m'
	export LESS_TERMCAP_us=$'\033[1;4;31m'
}
lessColors
unset lessColors


# fzf coloring
fzfConfig(){
  local color1=6
  local color2=10
  export FZF_DEFAULT_OPTS="
    --color fg:7,bg:-1,hl:${color1},fg+:7,bg+:-1,hl+:${color2}
    --color info:${color2},prompt:${color2},spinner:${color2}
    --color pointer:${color2},marker:${color2},header:${color2}
  "
  export FZF_DEFAULT_COMMAND="find . -type d \
          \( -name 'BraveSoftware' -o -path '*/dots/*' -o -path '*/.git/*' -o -path '*/_cacache/*' -o -path '*coc*' -o -path '*.cache*' \) \
          -prune -false -o -name '*'"
}
command -v fzf >/dev/null &&
fzfConfig
unset fzfConfig

# To bind Ctrl-l to type clear and then return/enter:
bind '"\C-l":"\033[4~\C-uclear\n"'

# LF Icons
command -v lf >/dev/null &&
export LF_ICONS="\
*=:\
tw=:\
st=:\
ow=:\
dt=:\
di=:\
fi=:\
ln=:\
or=:\
ex=:\
*.c=:\
*.h=:\
*.html=:\
*.lua=:\
*.md=:\
*.vim=:\
*.sh=:\
*.gz=:\
*.zip=:\
"

if command -v nnn >/dev/null; then
	export NNN_PLUG="g:getplugs;p:preview-tui;m:nmount"
	export NNN_BMS='d:~/Documents;t:/tmp;r:~/.local/rice'

	BLK="0B" CHR="0B" DIR="04" EXE="02" REG="00" HARDLINK="05" SYMLINK="06" MISSING="07" ORPHAN="09" FIFO="06" SOCK="03" OTHER="06"
	export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"
	export NNN_OPENER=opener
fi


if [ ! -f "$XDG_CONFIG_HOME"/git/config ]; then
	mkdir --parent "$XDG_CONFIG_HOME"/git
	touch "$XDG_CONFIG_HOME"/git/config
fi

printf ''
