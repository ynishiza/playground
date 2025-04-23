#!/usr/bin/env bash
set -eu -o pipefail

# shellcheck disable=SC2034
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC2034
__file="${__dir}/$(basename "${BASH_SOURCE[0]}")"

outdir="$__dir/out"
for inputfile in "$outdir"/*.dot
do
	filename=$(basename -s ".dot" "$inputfile")
	svgfile="$(dirname "$inputfile")/$filename.svg"
	dot -Tsvg -y "$inputfile"  > "$svgfile"
	echo "$svgfile"
done
