#!/bin/sh
# Interactive migration to be more smeueg instantly
# Trying to imitate ncurses using pure shell and ansi escape sequences

# Variables
DOTFILE_URL="https://gitlab.com/Smeueg/Dotfiles.git"
DOTFILE_DIR="${HOME}/.local/dots"
WALLPAPER_DIR="${HOME}/.local/rice/Wallpapers"

# Menus
main_choices='{0,Dotfiles},{0,Wallpaper},{0,*Suckless},{0,*Fonts},{0,Continue}'


# Access array "{}"
array()
(
	arr=${1}
	arr="${arr#\{}"
	arr="${arr%\}}"
	index=0

	IFS=,
	for i in ${arr}; do
		if [ ${index} -eq ${2} ] || [ ${2} -eq -1 ]; then
			i="${i# }"
			i="${i% }"
			echo "${i}"

			if [ ${index} -eq ${2} ]; then
				break
			fi
		fi

		index=$(($index + 1))
	done
)


get()
{
	url="${1}"
	file="${2}"

	if [ "$(command -v wget)" ]; then
		wget "${url}" -O "${file}"
	elif [ "$(command -v curl)" ]; then
		curl -L "${url}" -o "${file}"
	fi
}


Dotfiles()
{
	git clone --bare "${DOTFILE_URL}" "${DOTFILE_DIR}" ||
	{
		printf 'ERROR: Failed to run `git clone --bare "%s" "%s"`\n' "${DOTFILE_URL}" "${DOTFILE_DIR}" >&2
		return
	}

	old_ifs=${IFS}
	IFS='
	'

	for file in $(git --git-dir "${DOTFILE_DIR}" ls-tree -r HEAD | while read line; do printf '/%s\n' "${line##*$(printf '\t')}"; done); do
		if [ -f "${HOME}${file}" ]; then
			if [ ! -d "${HOME}/DotfilesBackup${file%/*}" ]; then
				mkdir --parents "${HOME}/DotfilesBackup${file%/*}"
			fi

			mv "${HOME}${file}" "${HOME}/DotfilesBackup"
		fi
	done

	git --git-dir="${DOTFILE_DIR}" --work-tree="${HOME}" checkout ||
	{
		printf 'ERROR: Failed to run `git --git-dir="%s" --work-tree="%s"`\n' "${DOTFILE_DIR}" "${HOME}" >&2
		return
	}

    git --git-dir="${DOTFILE_DIR}" config status.showUntrackedFiles no
		printf 'WARNING: Failed to run `git config --git-dir="%s" status.showUntrackedFiles no`\n' "${DOTFILE_DIR}" >&2

	IFS=${old_ifs}
}


Wallpaper()
{
	get "https://images2.alphacoders.com/950/950452.png" "${WALLPAPER_DIR%/}/Rock Thing.png"
	get "https://cdna.artstation.com/p/assets/images/images/004/085/784/large/starkiteckt-designs-untitled-2.jpg?1480219852" "${WALLPAPER_DIR%/}Red Space.jpg"
}



get_input()
{
	stty -icanon -echo
	input="$(dd bs=1 count=1 2>/dev/null)"

	if [ "${input}" = "$(printf '\033')" ]; then
		input=$(dd bs=1 count=1 2>&1)
		input=$(dd bs=1 count=1 2>&1)
		special=1
	fi

	case "${input%%1+0*}${special}" in
		h|D1) printf 'left'   ;;
		k|A1) printf 'up'     ;;
		j|B1) printf 'down'   ;;
		l|C1) printf 'right'  ;;
		a|A)  printf 'all'    ;;
		q)    printf 'quit'   ;;
		' '|'') printf 'select' ;;
	esac
}


quit()
{
	exit
}


choose()
{
	while true; do

	done
}



main()
{
	stty -echo
	if [ ! "$(command -v dd)" ]; then
		printf 'ERROR: This script requires `dd` to be installed, please install `dd` then run this script again\n' >&2
	fi

	choose "main_choices"
}
main
