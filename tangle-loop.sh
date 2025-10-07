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
	tangle || true
	if tangle; then
		if ((__test_hs)) && ! make -C "${SCRIPT_ROOT}" test; then
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
			make -C "${SCRIPT_ROOT}" lint || true
		fi
		sleep 1
	done
}

main()
{
	__test_hs=1
	if [[ "${1:-}" == "fe" ]]; then
		__test_hs=0
	fi

	watch \
		"${SCRIPT_ROOT}/base/lilac.org" \
		"${SCRIPT_ROOT}/base/compile.org" \
		"${SCRIPT_ROOT}/base/parse.org" \
		"${SCRIPT_ROOT}/base/tangle.org" \
		"${SCRIPT_ROOT}/base/types.org" \
		"${SCRIPT_ROOT}/base/weave.org" \
		"${SCRIPT_ROOT}/base/fe.org" \
		"${SCRIPT_ROOT}/base/artifacts/plan.org"
}

main "$@"
