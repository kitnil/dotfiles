#!/bin/sh

appname="$1"
summary="$2"
body="$3"
icon="$4"
urgency="$($5 | sed 's/critical//I')"

notify-send --urgency="$urgency" "$summary" "$body"
