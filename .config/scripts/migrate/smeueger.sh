#!/bin/sh
# Interactive migration to be more smeueg instantly.
# If you call now, you can be more smeueg with just $0.00
# Or you get 2 more smeueg with only $0.00

# Trying to imitate ncurses using pure shell and ansi escape sequences
# I miss arrays, please help

# Variables
DOTFILE_URL="https://gitlab.com/Smeueg/Dotfiles.git"
DOTFILE_DIR="${HOME}/.local/dots"
WALLPAPER_DIR="${HOME}/.local/rice/Wallpapers"

wallpapers="\
Geometry.jpg       https://images7.alphacoders.com/106/1065726.jpg
Rock Thing.png     https://images2.alphacoders.com/950/950452.png
Red Space.jpg      https://cdna.artstation.com/p/assets/images/images/004/085/784/large/starkiteckt-designs-untitled-2.jpg?1480219852
Warm Mountain.jpg  https://i.redd.it/odrdq4yqtoz61.jpg
"

# Menu
# Name   (dependencies)                        {c libraries}                             [Submenu]          "function/program to execute"
main_choices='
Dotfiles  (git)                                                                                              "get_dotfiles"
Wallpaper (wget||curl)                                                                                       "get_wallpapers"
Fonts     (wget||curl)                                                                                       "get_fonts"
Suckless  (git||tar,make,gcc||clang)  {X11,Xinerama,fontconfig,Xft,Xrender,Xext}     [suckless_choices]       "compile_suckless"
'

suckless_choices='
Dynamic-Window-Manager (git||tar) {X11,Xinerama,fontconfig,Xft} "get_suckless dwm"
DwmBlocks              (git||tar) {X11}
Simple-Terminal        (git||tar) {fontconfig,freetype2,harfbuzz}
D-Menu                 (git||tar) {fontconfig,xinerama,freetype2}
Slock                  (git||tar) {Xext,Xrandr,Imlib2,c,crypt,xft,xinerama}
External-X-Tools       (git||tar) {X11,Xft}
'


while [ "${1}" ]; do
	case "${1}" in
		'--help'|'-h')   help='true' ;;
		'--dryrun'|'-d') dry='true'  ;;
	esac
	shift
done


if [ "$(command -v gcc)" ]; then
	compiler='gcc'
elif [ "$(command -v clang)" ]; then
	compiler='clang'
elif [ "$(command -v cc)" ]; then
	compiler='cc'
else
	compiler=''
fi


if [ "${BASH}" ]; then
	bourne_shell='false'
	input_cmd='read -n1 input'
elif [ "${ZSH_NAME}" ]; then
	bourne_shell='false'
	input_cmd='read -k1 input'
else
	bourne_shell='true'
	input_cmd='input=$(dd bs=1 count=1 2>/dev/null)'
fi


old_ifs=${IFS}


# Utilies
sh_cat()
{
	while IFS= read -r line; do
		echo "${line}"
	done < "${1}"
}


get()
{
	url="${1}"

	if [ "$(command -v wget)" ]; then
		wget "${url}" -U 'Mozilla/5.0'
	elif [ "$(command -v curl)" ]; then
		curl -L "${url}" -A 'Mozilla/5.0'
	fi
}


get_input()
{
	eval "${input_cmd}"

	if [ "${input}" = "$(printf '\033')" ]; then
		eval "${input_cmd}"
		eval "${input_cmd}"
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


# Choice Functions #
get_dotfiles()
{
	if ! git clone --bare "${DOTFILE_URL}" "${DOTFILE_DIR}"; then
		printf 'ERROR: Failed to run `git clone --bare "%s" "%s"`\n' "${DOTFILE_URL}" "${DOTFILE_DIR}" >&2
		return
	fi

	IFS='
	'
	for file in $(git --git-dir "${DOTFILE_DIR}" ls-tree -r HEAD | while read -r line; do printf '/%s\n' "${line##*$(printf '\t')}"; done); do
		[ ! -f "${HOME}${file}" ] &&
			continue

		if [ ! -d "${HOME}/DotfilesBackup${file%/*}" ]; then
			if ! mkdir --parents "${HOME}/DotfilesBackup${file%/*}"; then
				printf "ERROR: Failed to create directory %s\n" "${HOME}/DotfilesBackup${file%/*}" >&2
				return
			fi
		fi

		mv "${HOME}${file}" "${HOME}/DotfilesBackup"
	done

	if ! git --git-dir="${DOTFILE_DIR}" --work-tree="${HOME}" checkout; then
		printf 'ERROR: Failed to run `git --git-dir="%s" --work-tree="%s"`\n' "${DOTFILE_DIR}" "${HOME}" >&2
		return
	fi

    git --git-dir="${DOTFILE_DIR}" config status.showUntrackedFiles no ||
		printf 'WARNING: Failed to run `git config --git-dir="%s" status.showUntrackedFiles no`\n' "${DOTFILE_DIR}" >&2
}


get_wallpapers()
{
	IFS='
	'

	for line in ${wallpapers}; do
		file_name="${line%https:*}"
		file_name="${file_name%${file_name##*[! ]}}"

		url="${line#*https}"
		url="https${url}"
		get "${url}" "${file_name}"
	done

	IFS=${old_ifs}
}


# Main Functions
check_depend()
{
	error='false'
	if [ ! "$(command -v stty)" ]; then
		printf 'ERROR: This script requires `stty` to be installed...\n' >&2
		error='true'
	fi

	if [ ! "$(command -v dd)" ] && [ ! "${BASH}" ] && [ ! "${ZSH_NAME}" ]; then
		printf "ERROR: This script requires \`dd\` to be installed if this script isn't executed using bash or zsh...\n" >&2
		error='true'
	fi

	if "${error}"; then
		exit -1
	fi
}


choose()
{
	variable="${1}"
	subchoice="${2}"
	message="${3}"
	eval "array=\$${variable}"
	IFS='
	'

	choices=$(
		for element in ${array}; do
			display='false'
			dependencies="${element#*(}"
			dependencies="${dependencies%)*}"

			# Check if required programs are installed
			IFS=,
			for dep in ${dependencies}; do
				display='true'
				if [ "${dep##*||*}" ]; then
					if [ ! "$(command -v ${dep})" ]; then
						display='false'
						break
					fi
					continue
				fi

				display='false'
				IFS='||'
				for dep in ${dep}; do
					if [ "$(command -v ${dep})" ]; then
						display='true'
						break
					fi
				done

				if ! "${display}"
				then break
				fi
			done

			# Check if libraries are installed
			if [ ! "${element##*\{*\}*}" ] && "${display}"; then
				if [ ! "${compiler}" ]; then
					display='false'
				else
					libraries="${element#*\{}"
					libraries="${libraries%\}*}"
					IFS=,
					for lib in ${libraries}; do
						lib=$(${compiler} --print-file-name="lib${lib}.so")
						if [ "${lib##/*/*.so}" ]; then
							display='false'
							break
						fi
					done
				fi
			fi

			if [ ! "${element##*\[*\]*}" ]; then
				if "${display}"
				then printf '2%s\n' "${element}"
				else printf '4%s\n' "${element}"
				fi
			else
				if "${display}"
				then printf '0%s\n' "${element}"
				else printf '3%s\n' "${element}"
				fi
			fi
		done

		if "${subchoice}"
		then printf '0Back\n'
		else printf '0Continue\n'
		fi
	)


	IFS='
	'

	selected=0
	while true; do
		index=0
		for choice in ${choices}; do
			choice="${choice%% *}"


			if [ "${index}" -eq "${selected}" ]; then
				printf '\033[7m' >&2
			fi

			if [ ! "${choice%%3*}" ]; then
				printf '\033[31m'
			fi

			case "${choice}" in
				*'Continue'*|*'Back'*) printf '  ' >&2   ;;
				4*)           printf '\033[31m *  ' >&2 ;;
				3*)           printf '\033[31m[ ] ' >&2 ;;
				2*)           printf ' *  ' >&2 ;;
				1*)           printf '[X] ' >&2 ;;
				*)            printf '[ ] ' >&2 ;;
			esac

			printf '%s\033[0m\n' "${choice#[0-4]}" >&2
			index=$((index + 1))
		done
		index=$((index - 1))

		case "$(get_input)" in
			'up')   [ "${selected}" -ne '0' ]        && selected=$((selected - 1)) ;;
			'down') [ "${selected}" -ne "${index}" ] && selected=$((selected + 1)) ;;
			'quit')
				eval "${variable}=''"
				printf "\033[$((index + 1))A\033[J" >&2
				return
				;;

			'select')
				# If selected Continue or back
				if [ "${selected}" -eq "${index}" ]; then
					printf "\033[$((index + 1))A\033[J" >&2

					tmp=$(
						for choice in ${choices}; do
							if [ ! "${choice##*Continue*}" ] || [ ! "${choice##*Back*}" ]; then
								continue
							fi
							herbe "${choice}" &
							printf '%s\n' "${choice}"
						done
					)
					eval "${variable}='${choices}'"
					return
				fi

				# If selected a submenu
				chosen_submenu=''
				index=0
				for choice in ${choices}; do
					if [ "${index}" -eq "${selected}" ] && [ ! "${choice##*\[*\]*}" ]; then
						chosen_submenu="${choice##*\[}"
						chosen_submenu="${chosen_submenu%%\]*}"
						break
					fi
					index=$((index + 1))
				done


				if [ "${chosen_submenu}" ]; then
					tmp_choices="${choices}"
					tmp_selected="${selected}"
					IFS='
					'
					printf "\033[$((index + 2))A\033[J" >&2
					choose "${chosen_submenu}" 'true'
					choices="${tmp_choices}"
					selected="${tmp_selected}"
					index=$((index + 1))
				else
					choices=$(
						index=0
						for choice in ${choices}; do
							if [ "${index}" = "${selected}" ]; then
								case "${choice}" in
									0*) printf '1%s\n' "${choice#?}" ;;
									1*) printf '0%s\n' "${choice#?}" ;;
								esac
							else
								printf '%s\n' "${choice}"
							fi
							index=$((index + 1))
						done
					)
				fi
				;;

			'all')
				choices=$(
					if [ "${choices%%1*}" ]; then
						num='1'
					else
						num='0'
					fi
					for choice in ${choices}; do
						if [ ! "${choice##?Continue}" ] || [ ! "${choice##?Back}" ] || [ ! "${choice##[234]*}" ]; then
							printf '%s\n' "${choice}"
						else
							printf '%i%s\n' "${num}" "${choice#?}"
						fi
					done
				)
				;;
		esac

		index=$((index + 1))
		printf "\033[${index}A"

	done
}


quit()
{
	stty echo
	stty icanon
	exit
}


# Main Function
main()
{
	if [ "${USER}" = "root" ]; then
		printf 'Are you sure you would like to run this script as root (not recommended)? [Y/n] '
		read reply
		case "${reply}" in
			[Yy][Ee][Ss]|[Yy]) ;;
			*)
				printf 'Exiting...\n'
				exit 1
				;;
		esac
	fi


	stty -echo
	"${bourne_shell}" &&
		stty -icanon

	trap 'quit' INT
	choose 'main_choices' 'false'

	IFS='
	'

	subchoices=''
	choice=''
	for choice in ${main_choices}; do
		if [ ! "${choice##*\[*\]*}" ]; then
			choice="${choice#*\[}"
			choice="${choice%\]*}"
			if [ "${subchoices}" ]; then
				subchoices="${subchoices}:${choice}"
			else
				subchoices="${choice}"
			fi
		fi
	done

	return

	IFS=:
	for choice in ${subchoices}; do
		echo "${choice}"
		return
		choose "${choice}"
	done

	IFS=${old_ifs}
	quit
}
main
