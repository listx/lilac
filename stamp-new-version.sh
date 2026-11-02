#!/usr/bin/env bash
set -euo pipefail

if awk '/^\*+ .*\yWIP\y/ { found=1; exit } END { exit !found }' release-notes.org; then
	echo >&2 "'WIP' found in release-notes.org heading; please update it first."
	exit 1
fi

if awk '/ COMMIT$/ { found=1; exit } END { exit !found }' release-notes.org; then
	echo >&2 "'COMMIT' found in release-notes.org; please update it first."
	exit 1
fi

version="$(awk \
	'/^#\+name: Lilac version$/{getline; getline; print}' index.org)"

if [[ -d .jj ]]; then
	if [[ -z "$(jj diff --summary)" ]]; then #
		echo >&2 "nothing to commit; please bump the version first."
		exit 1
	fi
	jj commit -m "Lilac ${version}" #
else
	git commit -am "Lilac ${version}"
fi
git tag -a "v${version}" -m "Lilac ${version}"
