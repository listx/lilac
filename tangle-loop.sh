#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

get_tangled_program_files()
{
	git -C "${SCRIPT_ROOT}" grep -E '^#\+header: :tangle' -- '*.org' \
		| awk '{print $3}' \
		| grep '\.\(cabal\|hs\|nix\|sh\)$'
}

clean_build_files()
{
	echo >&2 "Cleaning build files."
	make -C "${SCRIPT_ROOT}" clean
}

reset_tangled_program_files()
{
	__ok_to_lint=0
	echo >&2 "Error encountered; cleaning Haskell tangle paths."
	>/dev/null git checkout -- "${tangled_program_files[@]}" \
		Makefile \
		.gitignore \
		nix
}

reset_everything()
{
	reset_tangled_program_files
	clean_build_files
}

tangle()
{
	mapfile -t tangled_program_files < <(get_tangled_program_files) #
	if ! time make -C "${SCRIPT_ROOT}" tangle; then
		reset_everything
		return 1
	fi
	__ok_to_lint=1
	return 0
}

run_all_checks()
{
	tangle || true  #
	if tangle; then #
		if ((__test_hs)); then
			if ! make -C "${SCRIPT_ROOT}" test; then #
				reset_everything
				return
			else
				__ok_to_lint=1
			fi
		fi
	fi

	echo "----------------------------------------"

	if ! time make -C "${SCRIPT_ROOT}" weave; then
		reset_everything
		return
	else
		__ok_to_lint=1
	fi

	echo "----------------------------------------"

	if ((__ok_to_lint)); then
		make -C "${SCRIPT_ROOT}" lint || true
	fi
}

watch()
{
	local s1
	local s2
	local files=("$@")

	s1="$(stat -c %Y "${files[@]}")"

	run_all_checks #
	while true; do
		s2="$(stat -c %Y "${files[@]}")"
		if [[ ! "${s1}" == "${s2}" ]]; then
			s1="${s2}"
			run_all_checks
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
	__ok_to_lint=1

	readarray -t org_files < <(find base -type f -regex '.+\.org$')
	watch "${org_files[@]}"
}

main "$@"
