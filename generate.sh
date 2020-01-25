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

# curl --silent --request GET --user "admin:$(pass show jenkins/admin-api-key)" $JENKINS_URL/job/wigust/job/awesome/config.xml | xq --raw-output '.["org.jenkinsci.plugins.workflow.multibranch.WorkflowMultiBranchProject"].sources.data["jenkins.branch.BranchSource"].source.remote'

jobs()
{
    folder="$1"
    curl --silent --request POST --user "admin:$(pass show jenkins/admin-api-key)" "$JENKINS_URL/job/$folder/api/json"
}

for folder in $(folders); do
    for job in $(jobs $folder | jq --raw-output '.jobs[] | .name'); do
        giturl=$(curl --silent --request GET --user "admin:$(pass show jenkins/admin-api-key)" "$JENKINS_URL/job/$folder/job/$job/config.xml" | xq --raw-output '.["org.jenkinsci.plugins.workflow.multibranch.WorkflowMultiBranchProject"].sources.data["jenkins.branch.BranchSource"].source.remote')
        curl --silent --request GET --user "admin:$(pass show jenkins/admin-api-key)" "$JENKINS_URL/job/$folder/job/$job/api/json" | yq --arg GITURL "$giturl" --arg FOLDER "$folder" --arg JOB "$job" --yaml-output '[.jobs[] | {"job": { "name": "\($FOLDER)/\($JOB)", "project-type": "multibranch", "periodic-folder-trigger": "1d", "prune-dead-branches": true, "number-to-keep": "10", "days-to-keep": "10", "script-path": "Jenkinsfile", "scm": {"git": {"url": $GITURL}}}}]'
    done
done
