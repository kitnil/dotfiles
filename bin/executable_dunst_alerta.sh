#!/bin/sh

appname="$1"
summary="$2"
body="$3"
icon="$4"
urgency="$5"

alerta send \
       --resource YouTube \
       --event "live.${RANDOM}" \
       --environment Production \
       --service Google \
       --severity informational \
       --text "$summary" \
       --value live \
       --timeout 7200

