#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

tangled_files()
{
	unset GIT_LITERAL_PATHSPECS
	git -C "${SCRIPT_ROOT}" grep -E '^#\+header: :tangle' -- '*.org' \
		| awk '{print $3}' \
		| sort -u
}

tracked_files()
{
	git -C "${SCRIPT_ROOT}" ls-files \
		-- . \
		':(exclude,top).gitmodules' \
		':(exclude,top).vale/styles/.gitkeep' \
		':(exclude,top)README.org' \
		':(exclude,top)LICENSE' \
		':(exclude,top)base/*.html' \
		':(exclude,top)base/*.org' \
		':(exclude,top)base/image/PNG-Gradient.png' \
		':(exclude,top)base/js/lilac.js.injectme' \
		':(exclude,top)base/bib.json' \
		':(exclude,top)base/csl-styles' \
		':(exclude,top)nix/sources.json' \
		':(exclude,top)nix/sources.nix' \
		| sort -u
}

main()
{
	echo >&2 -n "$0: checking tangled vs tracked files... "

	# (ref:tangled-tracked-diff)
	if diff -u \
		<(printf '%s\n' "$(tangled_files)" | sort) \
		<(tracked_files); then

		echo >&2 "OK"
	else
		echo >&2 "$0: failed"
		return 1
	fi
}

main "$@"
