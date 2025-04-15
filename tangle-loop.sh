#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

main()
{
	local t1
	local t2

	t1=$(date -r "${SCRIPT_ROOT}/lilac.org" +%s)

	while true; do
		t2=$(date -r "${SCRIPT_ROOT}/lilac.org" +%s)
		if [[ ! "${t1}" == "${t2}" ]]; then
			t1="${t2}"
			time make -C "${SCRIPT_ROOT}" tangle
			echo
		fi
		sleep 1
	done
}

main "$@"
