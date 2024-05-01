#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

guix_version()
{
    ./pre-inst-env guix show "$GUIX_BUILD_PACKAGE" \
        | recsel -Pversion
}

nix_expr()
{
    cat <<EOF
(builtins.getFlake "nixpkgs").legacyPackages.$NIX_SYSTEM.$GUIX_BUILD_PACKAGE.version
EOF
}

nix_version()
{
    nix eval --raw --impure --expr "$(nix_expr)"
}

guix_version_output="$(guix_version)"
nix_version_output="$(nix_version)"

build()
{
    ./pre-inst-env guix build --no-grafts "$GUIX_BUILD_PACKAGE"
}

guix_location()
{
    ./pre-inst-env guix show "$GUIX_BUILD_PACKAGE" \
        | recsel -Plocation \
        | cut -d: -f1
}

guix_location_output="$(guix_location)"

message()
{
    cat <<EOF
gnu: ${GUIX_BUILD_PACKAGE}: Update to ${nix_version_output}.

* ${guix_location_output#"${PWD}/"} (${GUIX_BUILD_PACKAGE}): Update to ${nix_version_output}.
EOF

}

commit()
{
    git add "$guix_location_output"
    git commit -m "$(message)"
}

if [[ $guix_version_output == "$nix_version_output" ]]
then
    echo "$guix_version_output"
    if [[ $(git diff) == "" ]]
    then
        :
    else
        build
        commit
    fi
    exit 0
else
    echo "${guix_version_output} does not match ${nix_version_output}"
fi

sed -i "s@${guix_version_output}@${nix_version_output}@g" "$guix_location_output"

guix_build()
{
    ./pre-inst-env guix build -S "$GUIX_BUILD_PACKAGE"
}

set +e
guix_build_output="$(guix_build 2>&1)"

set -e

expected_hash="$(echo "$guix_build_output" | awk '/expected hash/ { print $NF }')"
actual_hash="$(echo "$guix_build_output" | awk '/actual hash/ { print $NF }')"

sed -i "s@${expected_hash}@${actual_hash}@" "$guix_location_output"

build
commit
