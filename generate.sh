#!/bin/sh

for folder in /tmp/j/*.json; do
    jq '[.jobs[] | {"job": { "name": .name, "description": .description, "project-type": "multibranch", "periodic-folder-trigger": "1d", "prune-dead-branches": true, "number-to-keep": "10", "days-to-keep": "10", "script-path": "Jenkinsfile", "scm": {"git": {"url": .url, "credentials-id": "gitlab-git"}}}}]' < $folder \
        | yq -y . \
        | sed 's@    git@    - git@g' \
        | sed 's|https://jenkins.intr/job/|git@gitlab.intr:|g' \
        | sed 's|/job||g' \
        | sed 's@        url:@          url:@g' \
        | sed 's@        credentials-id:@          credentials-id:@g' \
        | tee $(basename $folder | sed 's|json|yml|g')
done
