#!/usr/bin/env bash
set -euo pipefail

cited_shas()
{
	git log -1 --format=%b "$1" \
		| sed 's/^    .*//' \
		| awk 'BEGIN { RS = "" } { gsub(/\n */, " "); print }' \
		| grep -oE '\b[0-9a-f]{7,40} \(' \
		| cut -d' ' -f1
}

tag="$(git describe --abbrev=0)"
status=0

while read -r commit; do
	while read -r sha; do
		if ! git merge-base --is-ancestor "$sha" HEAD 2>/dev/null; then #
			echo >&2 "$(git log -1 --format='%h ("%s")' "$commit") cites ${sha}, which is not in the history."
			status=1
		fi
	done < <(cited_shas "$commit")
done < <(git rev-list "${tag}..HEAD")

exit "$status"
