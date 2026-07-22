#!/usr/bin/env bash
set -euo pipefail

if awk '/ COMMIT$/ { found=1; exit } END { exit !found }' release-notes.org; then
	echo >&2 "'COMMIT' found in release-notes.org; please update it first."
	exit 1
fi

version="$(awk \
	'/^#\+name: Lilac version$/{getline; getline; print}' index.org)"

git commit -am "Lilac ${version}"
git tag -a "v${version}" -m "Lilac ${version}"
