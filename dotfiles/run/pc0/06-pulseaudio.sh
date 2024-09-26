#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

pactl load-module module-native-protocol-tcp
pactl load-module module-ladspa-sink sink_name=compressor-stereo plugin=sc4_1882 label=sc4 control=1,1.5,401,-30,20,5,30
