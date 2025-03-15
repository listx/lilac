#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

# Generated files (and binary files) should be excluded from this check.
excluded=(
	image/PNG-Gradient.png
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

		# Skip excluded files.
		if [[ " ${excluded[*]} " =~ [[:space:]]${f}[[:space:]] ]]; then
			continue
		fi

		# Ignore the warning for "line" which we don't use; we do use it
		# in the sense that we check whether the overall "tail -c 1 ..."
		# command succeeds.
		#
		# shellcheck disable=SC2034
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
