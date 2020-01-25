#!/bin/sh

# This script will generate Jenkins jobs in YAML format, so you could
# use "jenkins-jobs" program to operate them as described in
# https://docs.openstack.org/infra/jenkins-job-builder/project_workflow_multibranch.html
#
# The script requires curl, jq, yq programs.
# Tested on Jenkins 2.204.1.
#
# You should set JENKINS_URL and CURL_USER to your credentials, then
# run the script, e.g. ./generate.sh

JENKINS_URL="https://jenkins.wugi.info"
CURL_USER="admin:$(pass show jenkins/admin-api-key)"

jenkins_curl()
{
    curl --silent --user "$CURL_USER" $@
}

folders()
{
    jenkins_curl --request POST "$JENKINS_URL/api/json"
}

jobs()
{
    folder="$1"
    jenkins_curl --request POST "$JENKINS_URL/job/$folder/api/json"
}

job()
{
    folder="$1"
    job="$2"
    jenkins_curl --request GET "$JENKINS_URL/job/$folder/job/$job/api/json"
}

giturl()
{
    folder="$1"
    job="$2"
    jenkins_curl --request GET "$JENKINS_URL/job/$folder/job/$job/config.xml"
}

main()
{
    for folder in $(folders | jq --raw-output '.jobs[] | .name'); do
        echo $folder | yq --yaml-output --arg FOLDER "$folder" '[{"job": {"name": $FOLDER, "project-type": "folder"}}]'
        for job in $(jobs $folder | jq --raw-output '.jobs[] | .name'); do
            giturl=$(giturl $folder $job | xq --raw-output '.["org.jenkinsci.plugins.workflow.multibranch.WorkflowMultiBranchProject"].sources.data["jenkins.branch.BranchSource"].source.remote')
            job $folder $job | yq --arg GITURL "$giturl" --arg FOLDER "$folder" --arg JOB "$job" --yaml-output '[.jobs[] | {"job": { "name": "\($FOLDER)/\($JOB)", "project-type": "multibranch", "periodic-folder-trigger": "1d", "prune-dead-branches": true, "number-to-keep": "10", "days-to-keep": "10", "script-path": "Jenkinsfile", "scm": [{"git": {"url": $GITURL}}]}}]'
        done
    done
}

# Entry point
main
