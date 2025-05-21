#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

# PowerDNS API configuration.  Override default values by passing them as
# environment variables.
#
# 'powerdns' is a Kubernetes service name.
# 'pdns' is Kubernetes namespace.
POWERDNS_API_URL="${POWERDNS_API_URL:-http://powerdns.pdns:8081}"
#
POWERDNS_API_KEY="${POWERDNS_API_KEY:-PowerDNSAPI}"
POWERDNS_ZONE_NAME="${POWERDNS_ZONE_NAME:-home.wugi.info.}"
POWERDNS_ZONE_NS1="${POWERDNS_ZONE_NS1:-ns1.home.wugi.info.}"
POWERDNS_ZONE_NS2="${POWERDNS_ZONE_NS2:-ns2.home.wugi.info.}"

# Log to a '/var/log/powerdns-majordomo-svc-ru.log' file.
mkdir -p /var/log
exec &> >(tee /var/log/powerdns-majordomo-svc-ru.log)

# Working directory which will be deleted after exiting.
workspace="$(mktemp -d -t "powerdns.XXXXXXXXXX")"
trap 'chmod -Rf +w "$workspace"; rm -rf "$workspace"' EXIT
cd "$workspace" || exit

# Zone configuration.
cat > zone.json <<EOF
{
    "kind": "Native",
    "masters": [],
    "name": "$POWERDNS_ZONE_NAME",
    "nameservers": [
        "$POWERDNS_ZONE_NS1",
        "$POWERDNS_ZONE_NS2"
    ]
}
EOF

if curl --silent \
        --fail \
        --header "X-API-Key: ${POWERDNS_API_KEY}" \
        "${POWERDNS_API_URL}/api/v1/servers/localhost/zones/${POWERDNS_ZONE_NAME}"
then
    printf "Zone already exists, skipping create.\n"
else
    # Create zone.
    curl --silent \
         --verbose \
         --header "X-API-Key: ${POWERDNS_API_KEY}" \
         --data @zone.json \
         "${POWERDNS_API_URL}/api/v1/servers/localhost/zones"
fi

soa()
{
    curl --silent \
         --fail \
         --header "X-API-Key: ${POWERDNS_API_KEY}" \
         "${POWERDNS_API_URL}/api/v1/servers/localhost/zones/${POWERDNS_ZONE_NAME}" \
        | jq --raw-output '.rrsets[] | select(.type == "SOA") | .records[0].content'
}

cat > soa.json <<EOF
{
  "rrsets": [
    {
      "name": "${POWERDNS_ZONE_NAME}",
      "type": "SOA",
      "ttl": 3600,
      "changetype": "REPLACE",
      "records": [
        {
          "content": "ns1.${POWERDNS_ZONE_NAME} support.${POWERDNS_ZONE_NAME} 2023011204 10800 3600 604800 3600"
        }
      ]
    }
  ]
}
EOF

if [[ $(soa) == *"${POWERDNS_ZONE_NAME} support.${POWERDNS_ZONE_NAME}"* ]]
then
    printf "Zone SOA correct, skipping patching.\n"
else
    curl --request PATCH \
         --data @soa.json \
         --header "X-API-Key: ${POWERDNS_API_KEY}" \
         "${POWERDNS_API_URL}/api/v1/servers/localhost/zones/${POWERDNS_ZONE_NAME}"
fi
