#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

# Generated files should be excluded from this check.
excluded=(
)

check_trailing_newlines()
{
	local res=0
	mapfile -t files < <(git -C "${SCRIPT_ROOT}" ls-tree -r HEAD \
		| awk '{print $2,$4}' \
		| grep '^blob' \
		| awk '{print $2}')

	for f in "${files[@]}"; do
		# Skip empty files.
		if [[ ! -s "$f" ]]; then
			continue
		fi

		if ! tail -c 1 "$f" | read -r line; then
			echo >&2 "$0: $f does not end in a trailing newline"
			res=1
		fi
	done
	return "${res}"
}

main()
{
	echo >&2 -n "$0: checking trailing newlines... "
	if check_trailing_newlines; then
		echo >&2 "OK"
	else
		echo >&2 "$0: failed"
		return 1
	fi
}

main "$@"
