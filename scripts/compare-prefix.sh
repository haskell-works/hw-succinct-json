#!/usr/bin/env bash

file1="$1"
file2="$2"

file1_size="$(wc -c "$file1" | sed 's|^ *||g' | cut -d ' ' -f 1)"

diff "$file1" <(head -c $file1_size "$file2") > /dev/null || {
  echo "Files $file1 and $file2 differ"
  exit 1
}
