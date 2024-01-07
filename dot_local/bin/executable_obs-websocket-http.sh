#!/bin/sh

set -o nounset -o errexit -o pipefail

API_URL="${API_URL:-http://192.168.0.144:4456}"
API_PASSWORD="aevoP4oh"

case "$1" in
    "edit")
        case "$2" in
            scene)
                curl --request POST \
                     --header "Authorization: ${API_PASSWORD}" \
                     --header "Content-type: application/json" \
                     --data "{\"sceneName\":\"$3\"}" "${API_URL}/emit/SetCurrentProgramScene"
                ;;
        esac
        ;;
esac

printf "\n"
