#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

is_wip()
{
	git show -s --format=%s HEAD \
		| grep -qi '^\(update\|wip\|fixup\|f \|s \)'
}

main()
{
	>/dev/null pushd "${SCRIPT_ROOT}"

	if is_wip; then
		echo >&2 "skipping (WIP commit)"
		return
	fi

	if [[ -n $(git status --porcelain) ]]; then
		echo >&2 "$0: dirty tree"
		return 1
	fi

	if make test; then
		echo >&2 "OK"
	else
		echo >&2 "$0: failed"
		return 1
	fi
}

main "$@"
