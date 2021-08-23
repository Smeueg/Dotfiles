#!/bin/sh
get_input()
{
	stty -icanon -echo
	input=$(dd bs=1 count=1 2>&1)
	stty icanon echo

	if [ "${input%%1+0*}" = $'\033' ]; then
		input=$(dd bs=1 count=1 2>&1)
		input=$(dd bs=1 count=1 2>&1)
		special=1
	fi

	case "${input%%1+0*}${special}" in
		h|D1) printf 'Left'   ;;
		k|A1) printf 'Up'     ;;
		j|B1) printf 'Down'   ;;
		l|C1) printf 'Right'  ;;
		' '|$'\n') printf 'Select' ;;
	esac
}



main()
{
	if [ ! "$(command -v dd)" ]; then
		printf 'ERROR: This script requires `dd` to be installed, please install `dd` then run this script again\n' >&2
	fi
}
main
