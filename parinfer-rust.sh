#!/usr/bin/env bash
set -euo pipefail

RET=0

main()
{
	for file in "$@"; do
		# shellcheck disable=SC2094
		if ! parinfer-rust <"$file" | diff -u "$file" - 2>&1; then
			RET=1
		fi
	done

	exit ${RET}
}

main "$@"
