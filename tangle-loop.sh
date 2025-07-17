#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

get_tangled_program_files()
{
	git -C "${SCRIPT_ROOT}" grep -P '^#\+header: :tangle' -- '*.org' \
		| awk '{print $3}' \
		| grep '\.\(cabal\|hs\|nix\|sh\)$'
}

reset_tangled_program_files()
{
	echo >&2 "Error encountered; cleaning Haskell tangle paths."
	>/dev/null git checkout -- "${tangled_program_files[@]}" \
		Makefile \
		.gitignore \
		nix
}

tangle()
{
	mapfile -t tangled_program_files < <(get_tangled_program_files)
	if ! time make -C "${SCRIPT_ROOT}" tangle; then
		reset_tangled_program_files
		return 1
	fi
	return 0
}

tangle_and_test_and_weave()
{
	tangle
	if tangle; then
		if ! make -C "${SCRIPT_ROOT}" test; then
			reset_tangled_program_files
		fi
	fi
	echo "----------------------------------------"
	if ! time make -C "${SCRIPT_ROOT}" weave; then
		reset_tangled_program_files
	fi
}

watch()
{
	local s1
	local s2
	local files=("$@")

	s1="$(stat -c %Y "${files[@]}")"

	tangle_and_test_and_weave
	while true; do
		s2="$(stat -c %Y "${files[@]}")"
		if [[ ! "${s1}" == "${s2}" ]]; then
			s1="${s2}"
			tangle_and_test_and_weave
		fi
		sleep 1
	done
}

main()
{
	watch \
		"${SCRIPT_ROOT}/base/lilac.org" \
		"${SCRIPT_ROOT}/base/lilac-compile.org" \
		"${SCRIPT_ROOT}/base/lilac-parse.org" \
		"${SCRIPT_ROOT}/base/lilac-tangle.org" \
		"${SCRIPT_ROOT}/base/lilac-types.org" \
		"${SCRIPT_ROOT}/base/lilac-weave.org" \
		"${SCRIPT_ROOT}/base/fe.org"
}

main "$@"
