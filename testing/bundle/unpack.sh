#!/bin/sh
set -eu

bundle="$1"

current_file=""

while IFS= read -r line || [ -n "$line" ]; do
  case "$line" in
    "### FILE:"*)
      current_file=$(echo "$line" | sed 's/^### FILE: //')
      mkdir -p "$(dirname "$current_file")"
      : > "$current_file"
      ;;
    *)
      if [ -n "$current_file" ]; then
        echo "$line" >> "$current_file"
      fi
      ;;
  esac
done < "$bundle"

