#!/bin/sh

# This script was inspired by one of the recent TerminalForLife live streams on
# youtube. For a predefined list of channels it downloads and reads the HTML
# page, looks for a specific text in the HTML page and outputs those channels
# containing a specific pattern in their HTML document. These channels are
# likely currently live streaming.
# Originally made by sanet
# Made into a bourne shell script by Smeueg

# Dependencies: curl or wget

# List of youtube channels
channel_list='
Zaney           : https://www.youtube.com/channel/UCmw-QGOHbHA5cDAvwwqUTKQ
TerminalforLife : https://www.youtube.com/c/TerminalforLife
TheLinuxCast    : https://www.youtube.com/c/TheLinuxCast
'

usage()
{
	while read line; do
		printf '%s\n' "${line}"
	done <<-EOF
		Usage: ${0##*/} [OPTS]
		  -h, --help               - Display this help information.
		  -C, --nocolor            - Omit ANSI color escape sequences.
		  -l, --link               - Display URL to YouTube channel.
	EOF
}

# Handle arguments
color='true'
link='false'
while [ "${@}" ]; do
	case "${1}" in
		'--nocolor'|'-C')
			color='false'
			;;

		'--help'|'-h')
			usage
			exit 0
			;;

		'--link'|'-l')
			link='true'
			;;

		*)
			printf 'ERROR: Invalid option %s\n' "${1}" >&2
			exit 1
			;;
	esac
	shift
done


# Setting colors if color is enabled
if "${color}"; then
	red='\033[38;5;1m'
	green='\033[38;5;2m'
	bold='\033[1m'
	reset='\033[0m'
else
	red=''
	green=''
	bold=''
	reset=''
fi


# Check for wget and curl
if command -v wget >/dev/null; then
	cmd="wget -U 'Mozilla/5.0' -qO -"
elif command -v curl >/dev/null; then
	cmd="curl -A 'Mozilla/5.0' --silent"
else
	printf 'ERROR: Both `wget` and `curl` is not installed. This script requires one of them to be installed.\n' >&2
	exit 1
fi

# Loop over each channel and check if they're live
old_ifs=${IFS}
IFS='
'
for channel in ${channel_list}; do
	url="${channel#*: }"

	live=$(
		eval "${cmd} ${url}" | while read line; do
			case "${line}" in
				*'"iconType":"LIVE"'*)
					printf 'true'
					break
					;;
			esac
		done
	)

	[ ! "${live}" ] && live='false'

	channel_name="${channel%% : *}"
	channel_name="${channel_name%${channel_name##*[! ]}}"
	if "${live}"; then
		printf "%s is ${green}LIVE!${reset}" "${channel_name}"
		if "${link}"; then
			printf "${green}{%s}${reset}" "${url}"
		fi
		printf '\n'
	else
		printf "%s is ${red}offline${reset}\n" "${channel_name}"
	fi
done
IFS=${old_ifs}
