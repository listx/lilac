#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

get_tangled_haskell_files()
{
	git -C "${SCRIPT_ROOT}" grep -P '^#\+header: :tangle' -- '*.org' \
		| awk '{print $3}' \
		| grep '\.\(hs\)$'
}

tangle()
{
	mapfile -t tangled_haskell_files < <(get_tangled_haskell_files)
	if ! time make -C "${SCRIPT_ROOT}" tangle; then
		echo >&2 "Tangle failed; cleaning Haskell tangle paths."
		>/dev/null git checkout -- "${tangled_haskell_files[@]}"
	fi
}

tangle_twice()
{
	tangle
	tangle
	echo "----------------------------------------"
}

main()
{
	local t1
	local t2

	t1=$(date -r "${SCRIPT_ROOT}/lilac.org" +%s)

	tangle_twice
	while true; do
		t2=$(date -r "${SCRIPT_ROOT}/lilac.org" +%s)
		if [[ ! "${t1}" == "${t2}" ]]; then
			t1="${t2}"
			tangle_twice
		fi
		sleep 1
	done
}

main "$@"
