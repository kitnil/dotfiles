#!/bin/sh

~/.guix-profile/bin/curl 'https://awx.wugi.info/api/v2/job_templates/9/launch/' -H 'Content-Type: application/json' -H "Authorization: Bearer $AWX_TOKEN" --data "{\"extra_vars\":{\"commit\":\"$(guix describe --format=recutils | recsel --expression='name="guix"' --print-row=commit)\"}}"
