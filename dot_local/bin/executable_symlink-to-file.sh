#!/usr/bin/env bash

set -o errexit -o pipefail

main()
{
    file_path="$1"
    file_backup_path="${1}.1"
    file_full_path="$(readlink -f "$file_path")"
    if [[ -h $file_path ]]
    then
        if [[ -e $file_backup_path ]]
        then
            exit 1
        fi
        mv --verbose --no-clobber "$file_path" "$file_backup_path"
        cp "$file_full_path" "$file_path"
        chmod u+w "$file_path"
    fi
}

main "$@"
