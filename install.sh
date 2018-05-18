#!/bin/bash
set -eu 
THIS_DIR=$(cd $(dirname $0); pwd)

for f in .??*
do
	[[ "$f" == ".git" ]] && continue
	[[ "$f" == ".DS_Store" ]] && continue

	echo "link $f"

	ln -snfv "$THIS_DIR"/"$f" "$HOME"
done
