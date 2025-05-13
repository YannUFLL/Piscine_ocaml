#!/usr/bin/env bash
set -euo pipefail

ROOT="."

find "$ROOT" -type l | while IFS= read -r link; do
    target="$(readlink -f "$link")"
    if [[ ! -f "$target" ]]; then
        echo "⚠️  Target not found or not a file: $link -> $target" >&2
        continue
    fi

    echo "🔗 $link → $target"
    rm "$link"
    cp "$target" "$link"
done

echo "✅ All links replaced by their pointed .ml file."
