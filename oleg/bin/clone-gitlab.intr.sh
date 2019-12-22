# curl -H "PRIVATE-TOKEN: $(pass show majordomo/gitlab.intr/tokens/terraform)" -H 'Content-Type: application/json' -k -X GET "https://gitlab.intr/api/v4/groups/$group/" | jq

# $(jq '.[] | .id' < groups.json)
for group in 7 8 17 70 71 82; do
    mkdir -p $group
    cd $group
    for project in $(curl -H 'PRIVATE-TOKEN: ***REMOVED***' -H 'Content-Type: application/json' -k -X GET https://gitlab.intr/api/v4/groups/$group/ | jq -r '.projects[] | .ssh_url_to_repo'); do
        git clone $project;
    done
    cd -
done
