#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

is_wip_commit_msg()
{
	cat "$@" \
		| grep -qi '^\(update\|wip\|fixup\|f \|s \)'
}

main()
{
	>/dev/null pushd "${SCRIPT_ROOT}"

	if is_wip_commit_msg "$@"; then
		echo >&2 "skipping (WIP commit)"
		return
	fi

	if grep --line-number '^FIXME' ./*.org; then
		echo >&2 -n "$0: FIXME marker(s) found; "
		echo >&2 "please remove marker(s) or prefix commit title with 'wip'"
		return 1
	fi

	echo >&2 "OK"
}

main "$@"
