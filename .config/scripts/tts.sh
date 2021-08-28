if [ "${1}" = "-k" ]; then
	output="$(pactl list modules short)"
	mic_modules=$(printf '%b\n' "${output}" | grep -Eo '.*mix-for-virtual-mic.*' | grep -Eo '^[0-9]+')
	silent_module=$(printf '%b\n' "${output}" | grep -Eo '.*silent-sink-for-echo-cancel.*' | grep -Eo '^[0-9]+')
	for module in ${mic_modules} ${silent_module}; do
		pactl unload-module ${module}
	done
	exit
fi


if [ ! "$(lsmod | grep -E '^snd_aloop')" ]; then
	printf 'WARNING: linux module snd_aloop is not loaded, please load the module\n'
	exit
fi


pactl load-module module-null-sink sink_name=mix-for-virtual-mic sink_properties=device.description=Mix-for-Virtual-Microphone

pactl load-module module-combine-sink sink_name=virtual-microphone-and-speakers \
slaves=mix-for-virtual-mic,alsa_output.pci-0000_00_1b.0.analog-stereo

pactl load-module module-loopback source=1 sink=mix-for-virtual-mic latency_msec=20

pactl load-module module-null-sink sink_name=silence sink_properties=device.description=silent-sink-for-echo-cancel

pactl load-module module-echo-cancel \
sink_name=virtual-microphone source_name=virtual-microphone \
source_master=mix-for-virtual-mic.monitor sink_master=silence aec_method=null \
source_properties=device.description=Virtual-Microphone \
sink_properties=device.description=Virtual-Microphone
