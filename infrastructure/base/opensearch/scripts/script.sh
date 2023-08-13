#!/bin/sh

set -o nounset -o errexit -o pipefail -o xtrace

username="${SCRIPT_OPENSEARCH_DASHBOARDS_USERNAME:-admin}"
password="${SCRIPT_OPENSEARCH_DASHBOARDS_PASSWORD:-admin}"
hostname="${SCRIPT_OPENSEARCH_DASHBOARDS_HOSTNAME:-opensearch-dashboards}"
port="${SCRIPT_OPENSEARCH_DASHBOARDS_PORT:-5601}"

users_and_passwors=(
    "${username}:${password}"
    "readall:readall"
)

# Return 0 if index pattern exists, other number otherwise.
index_pattern()
{
    curl --fail \
         --user "${username}:${password}" \
         --request GET \
         --silent \
         --verbose \
         "http://${hostname}:${port}/api/saved_objects/index-pattern/${1}"
}

for user_and_password in "${users_and_passwors[@]}"
do
    if index_pattern 'logstash-*'
    then
        echo "index-pattern 'logstash-*' exists, skipping creation." >&2
    else
        curl --user "$user_and_password" \
             --silent \
             --verbose \
             --request POST \
             "http://${hostname}:${port}/api/saved_objects/index-pattern/logstash-*" \
             --header "osd-xsrf:true" \
             --header "content-type:application/json" \
             --data '{"attributes": {"title": "logstash-*", "timeFieldName": "@timestamp"}}'
    fi
    # sampleSize
    # The number of rows to show in the table
    # Default: 500
    #
    # truncate:maxHeight
    # The maximum height that a cell in a table should occupy. Set to 0 to disable truncation
    # Default: 115
    curl --user "$user_and_password" \
         --silent \
         --verbose \
         --request POST \
         --header 'Content-Type: application/json' \
         --header "osd-xsrf:true" \
         --data-raw '{"changes":{"discover:sampleSize":10000,"truncate:maxHeight":0}}' \
         "http://${hostname}:${port}/api/opensearch-dashboards/settings"
done

# Return 0 if the policy exists, other number otherwise.
delete_old_indexes_policy()
{
    curl --fail \
         --user "${username}:${password}" \
         --request GET \
         --silent \
         --verbose \
         "http://${hostname}:${port}/api/ism/policies/delete_old_indexes"
}

if [[ $(delete_old_indexes_policy) == *'{"ok":true,"response":{"id":"delete_old_indexes"'* ]]
then
    echo "delete_old_indexes policy exists, skipping creation." >&2
else
    curl --user "${username}:${password}" \
         --silent \
         --verbose \
         --header "osd-version: 1.2.0" \
         "http://${hostname}:${port}/api/ism/policies/delete_old_indexes" \
         --request PUT \
         --header 'Content-Type: application/json' \
         --data-raw '
{
  "policy": {
    "description": "A simple default policy that deletes old indexes between hot and cold states.",
    "default_state": "hot",
    "states": [
      {
        "name": "hot",
        "actions": [],
        "transitions": [
          {
            "state_name": "cold",
            "conditions": {
              "min_index_age": "1d"
            }
          }
        ]
      },
      {
        "name": "cold",
        "actions": [
          {
            "delete": {}
          }
        ],
        "transitions": []
      }
    ],
    "ism_template": [
      {
        "index_patterns": [
          "filebeat-*",
          "logstash-*",
          "security-auditlog-*",
          "winlogbeat-*"
         ],
        "priority": 1
      }
    ]
  }
}
'
fi
