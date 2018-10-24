#!/usr/bin/env bash

methods="$1"
json_file="$2"

for method in $methods; do
  echo "== $method =="
  time hw-json create-index --method "$method" -i "$json_file" \
    --output-ib-file "$json_file-$method.ib.idx" \
    --output-bp-file "$json_file-$method.bp.idx"
done
