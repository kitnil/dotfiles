#!/bin/sh -e

 # Set up source from the OBS Monitor Output for Zoom Audio
pactl load-module module-null-sink \
sink_name=obs-monitor \
sink_properties=device.description=OBS_Monitor
pactl load-module module-remap-source \
master=obs-monitor.monitor \
source_name=obs-monitor-remap \
source_properties=device.description=OBS_Monitor

# Set up a delay line on the OBS Monitor Output to control audio
# latency into consumers of the OBS Monitor output. Clients can either
# pick off the non-delayed monitor output (obs-monitor-output) or the
# delayed monitor output (obs-monitor-delayed
#
#The sink that the delay line (delayed loopback) dumps into
pactl load-module module-null-sink \
sink_name=obs-monitor-delayed \
sink_properties=device.description=Delayed_OBS_Monitor

# The source carrying the audio dumped into the above sink for input
# into a client
pactl load-module module-remap-source \
source_name=obs-monitor-delayed \
master=obs-monitor-delayed.monitor \
source_properties=device.description=Delayed_OBS_Monitor

# The delay line itself taking from obs-monitor.monitor (the internal
# source that caries the OBS Monitor output) and delivering it to
# obs-monitor-delayed (the sink that will become the source for Zoom
# or other clients.
pactl load-module module-loopback \
latency_msec=300 \
source=obs-monitor.monitor \
source_dont_move=true \
sink=obs-monitor-delayed \
sink_dont_move=true \
sink_input_properties=device.description=OBS_Delay_Line_Loopback \
source_output_properties=device.description=OBS_Delay_Line_Loopback

# Set up a monitoring loopback so I can debug things with
# headphones... Default to the OBS monitor output delayed, since that
# should look and feel natural when using OBS.
pactl load-module module-loopback \
sink_input_properties=device.description=PA_Monitoring_Loopback \
source=obs-monitor-delayed.monitor \
source_output_properties=device.description=PA_Monitoring_Loopback 
