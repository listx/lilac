#!/usr/bin/env bash
set -euo pipefail

version="$(awk \
	'/^#\+name: Lilac version$/{getline; getline; print}' index.org)"

git commit -am "Lilac ${version}"
git tag -a "v${version}" -m "Lilac ${version}"
