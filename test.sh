#!/bin/sh
# Lint files with shellcheck in a DIRECTORY.
set -e
set -o pipefail

DIRECTORY="$1"

ERRORS=()

# find all executables and run `shellcheck`
for f in $(find "$DIRECTORY" -type f -not -path '*.git*' | sort -u); do
	if file "$f" | grep --quiet shell; then
		{
			shellcheck "$f" && echo "[OK]: successfully linted $f"
		} || {
			# add to errors
			ERRORS+=("$f")
		}
	fi
done

if [ ${#ERRORS[@]} -eq 0 ]; then
	echo "No errors, hooray"
else
	echo "These files failed shellcheck: ${ERRORS[*]}"
	exit 1
fi
