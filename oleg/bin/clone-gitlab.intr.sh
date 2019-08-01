# curl -H 'PRIVATE-TOKEN: ***REMOVED***' -H 'Content-Type: application/json' -k -X GET "https://gitlab.intr/api/v4/groups/$group/" | jq

for group in $(jq '.[] | .id' < groups.json); do mkdir -p $group; cd $group; git clone $(curl -H 'PRIVATE-TOKEN: ***REMOVED***' -H 'Content-Type: application/json' -k -X GET "https://gitlab.intr/api/v4/groups/$group/" | jq -r '.projects[] | .ssh_url_to_repo'); cd -; done
