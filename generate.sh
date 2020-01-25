#!/bin/sh

JENKINS_URL="https://jenkins.wugi.info"

folders()
{
    curl --silent --request POST --user "admin:$(pass show jenkins/admin-api-key)" "$JENKINS_URL/api/json" \
        | jq --raw-output '.jobs[] | .name'
}

# format()
# {
#     input=$1
#     echo $input
#         | sed 's@    git@    - git@g' \
#         | sed 's|https://jenkins.intr/job/|git@gitlab.intr:|g' \
#         | sed 's|/job||g' \
#         | sed 's@        url:@          url:@g' \
#         | sed 's@        credentials-id:@          credentials-id:@g' \
#         | tee $(basename $folder | sed 's|json|yml|g')
# }

for folder in $(folders); do
    curl --silent --request POST --user "admin:$(pass show jenkins/admin-api-key)" "$JENKINS_URL/job/$folder/api/json" \
        | yq --arg FOLDER "$folder" --yaml-output '[.jobs[] | {"job": { "name": "\($FOLDER)/\(.name)", "project-type": "multibranch", "periodic-folder-trigger": "1d", "prune-dead-branches": true, "number-to-keep": "10", "days-to-keep": "10", "script-path": "Jenkinsfile", "scm": {"git": {"url": .url}}}}]'
done
