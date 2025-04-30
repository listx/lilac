#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

get_tangled_haskell_files()
{
	git -C "${SCRIPT_ROOT}" grep -P '^#\+header: :tangle' -- '*.org' \
		| awk '{print $3}' \
		| grep '\.\(cabal\|hs\)$'
}

tangle()
{
	mapfile -t tangled_haskell_files < <(get_tangled_haskell_files)
	if ! time make -C "${SCRIPT_ROOT}" tangle; then
		echo >&2 "Tangle failed; cleaning Haskell tangle paths."
		>/dev/null git checkout -- "${tangled_haskell_files[@]}"
		return 1
	fi
	return 0
}

tangle_and_test()
{
	tangle
	if tangle; then
		make -C "${SCRIPT_ROOT}" test || true
	fi
	echo "----------------------------------------"
}

watch()
{
	local s1
	local s2
	local files=("$@")

	s1="$(stat -c %Y "${files[@]}")"

	tangle_and_test
	while true; do
		s2="$(stat -c %Y "${files[@]}")"
		if [[ ! "${s1}" == "${s2}" ]]; then
			s1="${s2}"
			tangle_and_test
		fi
		sleep 1
	done
}

main()
{
	watch \
		"${SCRIPT_ROOT}/lilac.org" \
		"${SCRIPT_ROOT}/fe.org"
}

main "$@"
